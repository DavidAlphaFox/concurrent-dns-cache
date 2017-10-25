{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache.Sync (
    ConcVar
  , newConcVar
  , wait
  , waitIncrease
  , decrease
  , ActiveVar
  , newActiveVar
  , tell
  , listen
  , ActiveRef
  , newActiveRef
  , lookupActiveRef
  , insertActiveRef
  , deleteActiveRef
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Network.DNS.Cache.Types
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)

----------------------------------------------------------------
-- 声明一个新的类型
newtype ConcVar = ConcVar (TVar Int)

-- 创建新的ConcVar
-- 外部嵌套了IO的Tvar
newConcVar :: IO ConcVar
newConcVar = ConcVar <$> newTVarIO 0

-- 原子操作
wait :: ConcVar -> (Int -> Bool) -> IO ()
wait (ConcVar var) cond = atomically $ do
    -- 读出Tvar中的数据
    -- 检查是否符合条件
    x <- readTVar var
    -- 如果成功就返回，否则重试
    check (cond x)

waitIncrease :: ConcVar -> Int -> IO ()
waitIncrease (ConcVar var) lim = atomically $ do
    x <- readTVar var
    -- 如果x是小于限制lim的时候
    check (x < lim)
    -- 使用Bang patterns
    -- 立刻进行计算
    let !x' = x + 1
    -- 写入Tvar
    writeTVar var x'

-- 直接减少1
decrease :: ConcVar -> IO ()
decrease (ConcVar var) = atomically $ modifyTVar' var (subtract 1)

----------------------------------------------------------------

newtype ActiveVar = ActiveVar (TMVar (Either DNSError Result))

newActiveVar :: IO ActiveVar
newActiveVar = ActiveVar <$> newEmptyTMVarIO

tell :: ActiveVar -> Either DNSError Result -> IO ()
tell (ActiveVar var) r = atomically $ putTMVar var r

-- 每次拿出来再放回去
listen :: ActiveVar -> IO (Either DNSError Result)
listen (ActiveVar var) = atomically $ readTMVar var

----------------------------------------------------------------

newtype ActiveRef = ActiveRef (IORef (Map Key ActiveVar))

newActiveRef :: IO ActiveRef
newActiveRef = ActiveRef <$> newIORef Map.empty

lookupActiveRef :: Key -> ActiveRef -> IO (Maybe ActiveVar)
lookupActiveRef key (ActiveRef ref) = Map.lookup key <$> readIORef ref

insertActiveRef :: Key -> ActiveVar -> ActiveRef -> IO ()
insertActiveRef key avar (ActiveRef ref) =
    atomicModifyIORef' ref $ \mp -> (Map.insert key avar mp, ())

deleteActiveRef :: Key -> ActiveRef -> IO ()
deleteActiveRef key (ActiveRef ref) =
    atomicModifyIORef' ref $ \mp -> (Map.delete key mp, ())
