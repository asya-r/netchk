module Connection.Types
  ( Result (..)
  , ResultFailure (..)
  , ResultSuccess (..)
  , showRes
  ) where

type Result = Either ResultFailure ResultSuccess

showRes :: Result -> String
showRes state = case state of
  Left e -> show e
  Right _ -> "Connection is established"

data ResultSuccess = ResultSuccess

data ResultFailure
  = ResultFailureBadURL
  | ResultFailureDNS String
  | ResultFailureTCP String
  | ResultFailureTLS String
  | ResultFailureHTTP String
  | ResultFailureUndefined String

instance Show ResultFailure where
  show s = case s of
    ResultFailureBadURL -> "Couldn't parse the given address"
    ResultFailureDNS _ -> "Configured DNS couldn't resolve the address"
    ResultFailureTCP _ -> "TCP handshake failed"
    ResultFailureTLS _ -> "TLS handshake failed"
    ResultFailureHTTP n -> "HTTP error, the status code is " ++ n
    ResultFailureUndefined _ -> "Connection failed, the problem is unknown"
