{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DNS.Cache (
    DNSCacheConf(..)
  , DNSCache
  , withDNSCache
  -- * Looking up
  , lookup
  , lookupCache
  -- * Resolving
  , Result(..)
  , resolve
  -- * Waiting
  , wait
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.IP (toHostAddress)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Network.DNS hiding (lookup)
import Network.DNS.Cache.Cache
import qualified Network.DNS.Cache.Sync as S
import Network.DNS.Cache.Types
import Network.DNS.Cache.Utils
import Network.DNS.Cache.Value
import Prelude hiding (lookup)

----------------------------------------------------------------

data DNSCacheConf = DNSCacheConf {
    resolvConfs    :: [ResolvConf]
  , maxConcurrency :: Int
  -- | Seconds.
  , minTTL         :: NominalDiffTime
  -- | Seconds.
  , maxTTL         :: NominalDiffTime
  }

data DNSCache = DNSCache {
    cacheSeeds      :: [ResolvSeed]
  , cacheNofServers :: !Int
  , cacheRef        :: CacheRef
  , cacheActiveRef  :: IORef (Map Key S.ActiveVar)
  , cacheConcVar    :: S.ConcVar
  , cacheConcLimit  :: Int
  , cacheMinTTL     :: NominalDiffTime
  , cacheMaxTTL     :: NominalDiffTime
  }

----------------------------------------------------------------

withDNSCache :: DNSCacheConf -> (DNSCache -> IO a) -> IO a
withDNSCache conf func = do
    seeds <- mapM makeResolvSeed (resolvConfs conf)
    let n = length seeds
    cacheref <- newCacheRef
    activeref <- newIORef Map.empty
    lvar <- S.newConcVar
    let cache = DNSCache seeds n cacheref activeref lvar maxcon minttl maxttl
    void . forkIO $ prune cacheref
    func cache
  where
    maxcon = maxConcurrency conf
    minttl = maxTTL conf
    maxttl = maxTTL conf

----------------------------------------------------------------

lookupPSQ :: DNSCache -> Domain -> IO (Key, Maybe (Prio, Value))
lookupPSQ cache dom = do
    !mx <- lookupCacheRef key cacheref
    return (key,mx)
  where
    cacheref = cacheRef cache
    !key = newKey dom

----------------------------------------------------------------

-- | Lookup 'Domain' only in the cache.
lookupCache :: DNSCache -> Domain -> IO (Maybe HostAddress)
lookupCache cache dom = do
    (_, mx) <- lookupPSQ cache dom
    case mx of
        Nothing    -> return Nothing
        Just (_,v) -> Just <$> rotate v

----------------------------------------------------------------

lookup :: DNSCache -> Domain -> IO (Maybe HostAddress)
lookup cache dom = fromEither <$> resolve cache dom

----------------------------------------------------------------

-- | Lookup 'Domain' in the cache.
--   If not exist, queries are sent to DNS servers and
--   resolved IP addresses are cached.
resolve :: DNSCache -> Domain -> IO (Either DNSError Result)
resolve _     dom
  | isIPAddr dom            = return $ Right $ Numeric $ tov4 dom
  where
    tov4 = read . BS.unpack
resolve cache dom = do
    (key,mx) <- lookupPSQ cache dom
    case mx of
        Just (_, v)         -> Right . Hit <$> rotate v
        Nothing -> do
            m <- readIORef activeref
            case Map.lookup key m of
                Just avar -> S.listen avar
                Nothing   -> do
                    avar <- S.newActiveVar
                    atomicModifyIORef' activeref $
                        \mp -> (Map.insert key avar mp, ())
                    x <- sendQuery cache dom
                    !res <- case x of
                        Left e      -> return $ Left e
                        Right addrs -> insert cache key addrs
                    atomicModifyIORef' activeref $
                        \mp -> (Map.delete key mp, ())
                    S.tell avar res
                    return res
  where
    activeref = cacheActiveRef cache

insert :: DNSCache -> Key -> [(HostAddress, TTL)] -> IO (Either DNSError Result)
insert _     _   []                   = return $ Left UnexpectedRDATA
insert cache key addrs@((addr,ttl):_) = do
    !val <- newValue $ map fst addrs
    !tim <- addUTCTime lifeTime <$> getCurrentTime
    insertCacheRef key tim val cacheref
    return $! Right $ Resolved addr
  where
    minttl = cacheMinTTL cache
    maxttl = cacheMaxTTL cache
    !lifeTime = minttl `max` (maxttl `min` fromIntegral ttl)
    cacheref = cacheRef cache

----------------------------------------------------------------

sendQuery :: DNSCache -> Domain -> IO (Either DNSError [(HostAddress,TTL)])
sendQuery cache dom = bracket setup teardown body
  where
    setup = waitIncrease cache
    teardown _ = decrease cache
    body _ = concResolv cache dom

waitIncrease :: DNSCache -> IO ()
waitIncrease cache = S.waitIncrease lvar lim
  where
    lvar = cacheConcVar cache
    lim = cacheConcLimit cache

decrease :: DNSCache -> IO ()
decrease cache = S.decrease lvar
  where
    lvar = cacheConcVar cache

concResolv :: DNSCache -> Domain -> IO (Either DNSError [(HostAddress,TTL)])
concResolv cache dom = withResolvers seeds $ \resolvers -> do
    eans <- resolv n resolvers dom
    return $ case eans of
        Left  err -> Left err
        Right ans -> fromDNSFormat ans getHostAddressandTTL
  where
    n = cacheNofServers cache
    seeds = cacheSeeds cache
    isA r = rrtype r == A
    unTag (RD_A ip) = ip
    unTag _         = error "unTag"
    toAddr = toHostAddress . unTag . rdata
    hostAddressandTTL r = (toAddr r, rrttl r)
    getHostAddressandTTL = map hostAddressandTTL . filter isA . answer

resolv :: Int -> [Resolver] -> Domain -> IO (Either DNSError DNSFormat)
resolv 1 resolvers dom = lookupRaw (head resolvers) dom A
resolv _ resolvers dom = do
    asyncs <- mapM async actions
    snd <$> waitAnyCancel asyncs
  where
    actions = map (\res -> lookupRaw res dom A) resolvers

----------------------------------------------------------------

-- | Wait until the predicate in the second argument is satisfied.
--   The predicate are given the number of the current resolving domains.
--
-- For instance, if you ensure that no resolvings are going on:
--
-- > wait cache (== 0)
--
-- If you want to ensure that capability of concurrent resolving is not full:
--
-- > wait cache (< maxCon)
--
-- where 'maxCon' represents 'maxConcurrency' in 'DNSCacheConf'.
wait :: DNSCache -> (Int -> Bool) -> IO ()
wait cache cond = S.wait lvar cond
  where
    lvar = cacheConcVar cache

prune :: CacheRef -> IO ()
prune cacheref = forever $ do
    threadDelay 10000000
    tim <- getCurrentTime
    pruneCacheRef tim cacheref
