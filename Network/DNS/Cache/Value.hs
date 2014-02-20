{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache.Value where

import Data.Array.Unboxed
import Data.IORef (newIORef, atomicModifyIORef')
import Network.DNS.Cache.Types

newValue :: [HostAddress] -> IO Value
newValue addrs = do
    ref <- newIORef next
    return $! Value arr ref
  where
    !siz = length addrs
    !next = adjust 0 siz
    !arr = listArray (0,siz-1) addrs

rotate :: Value -> IO HostAddress
rotate (Value a ref) = do
    let (_, siz) = bounds a
    j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
    let !addr = a ! j
    return addr

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = let !x = (i + 1) `mod` n in x
