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
  Right _ -> "Success"

data ResultSuccess = ResultSuccess

data ResultFailure
  = ResultFailureDNS String
  | ResultFailureTCP String
  | ResultFailureTLS String
  | ResultFailureHTTP String
  | ResultFailureBadURL
  | ResultFailureUndefined String

instance Show ResultFailure where
  show s = case s of
    ResultFailureDNS _ -> "DNS error"
    ResultFailureTCP _ -> "TCP error"
    ResultFailureTLS _ -> "TLS error"
    ResultFailureHTTP n -> "HTTP error, status code was " ++ n
    ResultFailureBadURL -> "Bad URL"
    ResultFailureUndefined _ -> "Undefined error"
