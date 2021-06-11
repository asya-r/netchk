{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Bandwidth.Check
  ( measure
  ) where
import Bandwidth.Types (Segment (..), FormattedSegment (..), Measurement (..), format)
import Lib.ProcNet (SockInfo (..), Addr4(..), readProcNetTcp4)

import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as BS
import Control.Lens.Internal.ByteString (unpackStrict8)
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as S
import qualified Control.Logging as CL (log, withStdoutLogging)
import Data.ByteString (ByteString)
import Network.HTTP.Client (Request, parseRequest, host, port, path, secure)
import qualified Network.Simple.TCP.TLS as TLS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Time (getCurrentTime)
import Data.List.Extra (snoc)
import qualified Data.Text as T
import Data.Bits.Extras (w16)
import Control.Monad (liftM2)
import Data.Word (Word16)
import qualified Network.Pcap as Pcap
import Control.Exception (bracket)
import Control.Lens.Internal.ByteString (unpackStrict8)
import Data.Fixed (showFixed)

measure :: String -> String -> IO Measurement
measure url interface = do
  request <- parseRequest url
  measurement <- bracket (openHandle interface request) closeHandle (work request)
  return measurement
  where
    openHandle interface request = do
      h <- Pcap.openLive interface 120 True 0
      let pcapFilt = concat [ "((src host ", (unpackStrict8 $ host request), ") and ", "(src port ", (show $ port request), "))", " or "
                            , "((dst host ", (unpackStrict8 $ host request), ") and ", "(dst port ", (show $ port request), "))"]
      Pcap.setFilter h pcapFilt False 0
      return h
    closeHandle h = do
      stat <- Pcap.statistics h
      print stat
      return stat
    work request h = test h request

test :: Pcap.PcapHandle -> Request -> IO Measurement
test phandle request = CL.withStdoutLogging $ do
  segments <- case secure request of
    True -> do
      service <- TLS.newDefaultClientParams (BS8.unpack $ host request, BS8.pack $ show $ port request)
      TLS.connect service (BS8.unpack $ host request) (show $ port request) passoff
    False -> CL.withStdoutLogging $ do
      let hints = S.defaultHints { S.addrFlags = [], S.addrSocketType = S.Stream }
      addr:_ <- S.getAddrInfo (Just hints) (Just $ BS8.unpack $ host request) (Just $ show $ port request)
      sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
      S.connect sock $ S.addrAddress addr
      NBS.send sock $ BS8.pack $ "GET " ++ (unpackStrict8 $ path request) ++ " HTTP/1.0\r\nContent-Type: text/plain\r\n\r\n"
      t <- getCurrentTime
      segments <- receiveAll sock False [Segment 0 t]
      sockAddr <- S.getPeerName sock
      rto <- retransmittedPackages sockAddr
      let segmentsFormatted = format segments
      return $ Measurement { measurementSegments = segmentsFormatted, measurementSpeed = speed segmentsFormatted, measurementPacketLoss = Nothing, measurementRTO = rto}
  return segments
  where
    passoff (context, sockAddr) = do
      TLS.send context $ BS8.pack $ "GET " ++ (unpackStrict8 $ path request) ++ " HTTP/1.0\r\nContent-Type: text/plain\r\n\r\n"
      t <- getCurrentTime
      segments <- receiveAllTLS context False [Segment 0 t]
      rto <- retransmittedPackages sockAddr
      let segmentsFormatted = format segments
      return $ Measurement { measurementSegments = segmentsFormatted, measurementSpeed = speed segmentsFormatted, measurementPacketLoss = Nothing, measurementRTO = rto}

receiveAndMeasure :: S.Socket -> IO (ByteString, Segment)
receiveAndMeasure sock = do
 chunk <- NBS.recv sock defaultChunkSize
 t <- getCurrentTime
 return (chunk, Segment (BS.length chunk) t)


receiveAll :: S.Socket -> Bool -> [Segment] -> IO [Segment]
receiveAll sock True segments = do
  return segments
receiveAll sock False segments = do
  (bs, seg) <- receiveAndMeasure sock
  s <- receiveAll sock (BS.null bs) (snoc segments seg)
  return s

receiveAndMeasureTLS :: TLS.Context -> IO (Bool, Segment)
receiveAndMeasureTLS context = do
 chunk <- TLS.recv context
 t <- getCurrentTime
 case chunk of
   Nothing -> return (True, Segment 0 t)
   Just chunk -> return (False, Segment (BS.length chunk) t)

receiveAllTLS :: TLS.Context -> Bool -> [Segment] -> IO [Segment]
receiveAllTLS _ True segments = return segments
receiveAllTLS context False segments = do
  (bs, seg) <- receiveAndMeasureTLS context
  s <- receiveAllTLS context (bs) (snoc segments seg)
  return s

retransmittedPackages :: S.SockAddr -> IO Word16
retransmittedPackages sockAddr = do
  sockInfo <- readProcNetTcp4
  let identicalPort = \x -> (siRemotePort x) == (w16 $ sockAddrToPort sockAddr)
      identicalHost = \x -> (w32 $ siRemoteAddress x) == (sockAddrToHost sockAddr)
      filtered = filter (liftM2 (&&) identicalPort identicalHost) sockInfo
  return $ case filtered of
    [] -> 0
    otherwise -> siRetransmitted $ head filtered
  where
    w32 (Addr4 a) = a
    sockAddrToPort (S.SockAddrInet port _) = port
    sockAddrToHost (S.SockAddrInet _ host) = host

speed :: [FormattedSegment] -> Float
speed segments = (kbSinceStart $ last segments) / (read $ showFixed True $ sSinceStart $ last segments)
