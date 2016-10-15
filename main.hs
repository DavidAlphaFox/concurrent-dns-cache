{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException(..))
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as BS
import Network.DNS
import Network.DNS.Cache as DNSC
import Data.Time

confs ::  [ResolvConf]
confs = [
    defaultResolvConf { resolvInfo = RCHostName "8.8.8.8" }
  , defaultResolvConf { resolvInfo = RCHostName "8.8.4.4" }
  ]

maxCon :: Int
maxCon = 50

cacheConf :: DNSCacheConf
cacheConf = DNSCacheConf {
    resolvConfs    = confs
  , maxConcurrency = maxCon
  , minTTL         = 60
  , maxTTL         = 300
  , negativeTTL    = 300
  }

main :: IO ()
main = do
    beg <- getCurrentTime
    withDNSCache cacheConf (loop 1 beg)
 where
   loop :: Int -> UTCTime -> DNSCache -> IO ()
   loop n beg cache = do
       -- 每循环1000次就重新获取下当前时间
       when (n `mod` 1000 == 0) $ do
           cur <- getCurrentTime
           putStrLn $ show n ++ ": " ++ show (cur `diffUTCTime` beg)
       -- 获取需要查询的域名
       edom <- try BS.getLine
       case edom of
           Left (SomeException _) -> do
               wait cache (== 0)
               putStrLn "Done."
           Right dom -> do
               wait cache (< maxCon)
               void $ forkIO (DNSC.resolve cache dom >>= p dom)
               loop (n+1) beg cache
   p _   (Right _) = return ()
   p dom (Left  e) = do
       putStr $ show e ++ " "
       BS.putStrLn dom
