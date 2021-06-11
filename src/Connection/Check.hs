{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Connection.Check
  ( checkConnection
  ) where

import Connection.Types (Result (..), ResultFailure (..), ResultSuccess (..))
import Config (CheckParameters (..), getHardcodedParameters)

import Control.Exception (try, SomeException)
import Network.HTTP.Client (parseRequest, newManager, responseStatus, httpLbs, managerResponseTimeout,
 responseTimeoutMicro, HttpException, Request, Response, host, secure, port)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as DB
import Network.DNS.Lookup (lookupA)
import Network.DNS.Resolver (withResolver, defaultResolvConf, makeResolvSeed, resolvTimeout)
import Network.Socket (HostName, PortNumber)
import qualified Network.Socket as Socket
import Network.Connection (connectTo, initConnectionContext, ConnectionParams (..), Connection)
import System.Timeout (timeout)
import qualified Network.TLS as TLS
import qualified System.X509
import Network.TLS.Extra.Cipher (ciphersuite_default)

checkConnection :: String -> IO Result
checkConnection url = do
  state <- checkURL url
  return state

checkURL :: String -> IO Result
checkURL url = do
  erequest <- try $ parseRequest url
  case erequest of
    Left (e :: HttpException) -> return $ Left $ ResultFailureBadURL
    Right request -> do
      state <- checkResponse request
      return state

checkResponse :: Request -> IO Result
checkResponse request = do
  manager <- newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro $ checkParametersHTTPTimeout getHardcodedParameters }
  eresponse <- try $ httpLbs request manager
  case eresponse of
    Left (e :: HttpException) -> do
      state <- checkDNS request
      return state
    Right response -> do
      state <- checkHTTPCode response
      return state

checkHTTPCode :: Response b -> IO Result
checkHTTPCode response = do
  let code = statusCode $ responseStatus response
  case code of
    200 -> return $ Right ResultSuccess
    n   -> return $ Left $ ResultFailureHTTP (show n)

checkDNS :: Request -> IO Result
checkDNS request = do
  rs <- makeResolvSeed defaultResolvConf { resolvTimeout = checkParametersDNSTimeout getHardcodedParameters }
  eips <- withResolver rs $ \resolver -> lookupA resolver $ host request -- TODO: it checks only IPv4
  case eips of
    Left e  -> return $ Left $ ResultFailureDNS $ show e
    Right _ -> do
      state <- checkTCP request
      return state

checkTCP :: Request -> IO Result
checkTCP request = do
  let (host, port) = devideRequest request
      connParams = ConnectionParams
         { connectionHostname = host
         , connectionPort = port
         , connectionUseSecure = Nothing
         , connectionUseSocks = Nothing }
  context <- initConnectionContext
  maybeConn <- timeout (checkParametersTCPTimeout getHardcodedParameters) $ try $ connectTo context connParams
  case maybeConn of
    Just (conn :: Either SomeException Connection) -> case conn of
      Left e -> return $ Left $ ResultFailureTCP (show e)
      Right _ -> do
        state <- checkTLS request
        return state
    Nothing -> return $ Left $ ResultFailureTCP ""

checkTLS :: Request -> IO Result
checkTLS request = do
  let (host, port) = devideRequest request
      hints = Socket.defaultHints
        { Socket.addrSocketType = Socket.Stream }
  addrInfos <- Socket.getAddrInfo (Just hints) (Just host) (Just $ show port)
  case addrInfos of
      [] -> return $ Left $ ResultFailureTCP ""
      addr:_ -> do
          sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
          Socket.connect sock (Socket.addrAddress addr)
          params <- tlsParams host
          ctx <- TLS.contextNew sock params
          h <- try $ TLS.handshake ctx
          case h of
            Left (e :: TLS.TLSException) -> return $ Left $ ResultFailureTLS (show e)
            Right _  -> return $ Left $ ResultFailureUndefined ""

devideRequest :: Request -> (HostName, PortNumber)
devideRequest request = (DB.unpack $ host request, fromIntegral $ port request)

tlsParams :: HostName -> IO TLS.ClientParams
tlsParams hostname = do
    store <- System.X509.getSystemCertificateStore
    let cp = TLS.defaultParamsClient hostname ""
    pure $ cp
        { TLS.clientShared = (TLS.clientShared cp)
            { TLS.sharedCAStore = store }
        , TLS.clientSupported = (TLS.clientSupported cp)
            { TLS.supportedCiphers = ciphersuite_default }
        }
