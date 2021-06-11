{-# LANGUAGE OverloadedStrings #-}
-- modified proc-net public package
module Lib.ProcNet (
  -- * Types
  SockInfo (..),
  Addr4 (..),
  Addr6 (..),
  Addr,
  -- * Reading
  readProcNet,
  readProcNetTcp4,
  readProcNetTcp6,
  readProcNetUdp4,
  readProcNetUdp6
  ) where

import Control.Monad
import Data.Attoparsec.ByteString
import Data.List
import Data.Word
import qualified Data.ByteString as B
import System.Posix.Types

data SockInfo addr = SockInfo {
  siLocalAddress :: addr,
  siLocalPort :: Word16,
  siRemoteAddress :: addr,
  siRemotePort :: Word16,
  siRetransmitted :: Word16,
  siUserId :: UserID,
  siInode :: FileID
  } deriving Show

-- | like Network.Socket.HostAddress
newtype Addr4 = Addr4 Word32
              deriving Show

-- | like Network.Socket.HostAddress6
newtype Addr6 = Addr6 (Word32, Word32, Word32, Word32)
              deriving Show

class Addr addr where
  parseAddr :: Parser addr

instance Addr Addr4 where
  parseAddr = liftM Addr4 $ hexBytes 4 id

instance Addr Addr6 where
  parseAddr = do
    let turn (a:b:as) = turn as ++ [a,b]
        turn [] = []
    [a1,a2,a3,a4] <- forM [1..4] $ \_ -> hexBytes 4 turn
    return $ Addr6 (a1, a2, a3, a4)

hexBytes :: Integral n => Int -> ([Word8] -> [Word8]) -> Parser n
hexBytes n f = do
  ds <- forM [1..n*2] $ \_ -> do
    b <- satisfy $ inClass "0-9A-F"
    if b >= 65
      then return (b - 65 + 10) -- 65 is ASCII A
      else return (b - 48)      -- 48 is ASCII 0
  return $ foldl' (\l r -> l*16 + fromIntegral r) 0 $ f ds

decNum :: Integral n => Parser n
decNum = do
  ds <- many1 $ satisfy $ inClass "0-9"
  return $ foldl' (\l r -> l*10 + fromIntegral r - 48) 0 ds

-- | Get the table from /proc/net/tcp
readProcNetTcp4 :: IO [SockInfo Addr4]
readProcNetTcp4 = readProcNet "/proc/net/tcp"

-- | Get the table from /proc/net/tcp6
readProcNetTcp6 :: IO [SockInfo Addr6]
readProcNetTcp6 = readProcNet "/proc/net/tcp6"

-- | Get the table from /proc/net/udp
readProcNetUdp4 :: IO [SockInfo Addr4]
readProcNetUdp4 = readProcNet "/proc/net/udp"

-- | Get the table from /proc/net/udp6
readProcNetUdp6 :: IO [SockInfo Addr6]
readProcNetUdp6 = readProcNet "/proc/net/udp6"

-- | Read a table from /proc/net (tcp or udp, 4 or 6)13
readProcNet :: Addr addr => FilePath -> IO [SockInfo addr]
readProcNet fp = do
  f <- B.readFile fp
  case parseOnly (readProcNet' <* endOfInput)  f of
    Left e -> error e
    Right a -> return a

readProcNet' :: Addr addr => Parser [SockInfo addr]
readProcNet' = do
  many1 $ notWord8 10
  word8 10
  many' readSockInfo

readSockInfo :: Addr addr => Parser (SockInfo addr)
readSockInfo = do
  -- sl
  skipWhite
  skipNum
  word8 colon
  -- local_address
  skipWhite
  laddr <- parseAddr
  word8 colon
  lport <- hexBytes 2 id
  -- remote_address
  skipWhite
  raddr <- parseAddr
  word8 colon
  rport <- hexBytes 2 id
  -- st
  skipWhite
  skipHex
  -- tx_queue
  skipWhite
  skipHex
  -- rx_queue
  word8 colon
  skipHex
  -- tr tm->when
  skipWhite
  skipHex
  word8 colon
  skipHex
  -- retrnsmt
  skipWhite
  retrnsmt <- decNum
  -- uid
  skipWhite
  uid <- decNum
  -- timeout
  skipWhite
  decNum
  -- inode
  skipWhite
  inode <- decNum
  -- trash
  many1 $ notWord8 10
  word8 10
  return $ SockInfo laddr lport raddr rport retrnsmt uid inode
  where skipWhite = many1 $ word8 32 -- 32 is ASCII white space
        isDigit w = w >= 48 && w <= 57
        skipNum = many1 $ skip isDigit
        skipHex = many1 $ skip $ inClass "0-9A-F"
        colon = 58 :: Word8
