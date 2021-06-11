module Config
  ( getHardcodedUrls
  , urlBandwidthTest
  , CheckParameters (..)
  , getHardcodedParameters
  ) where

getHardcodedUrls :: [String]
getHardcodedUrls =
  [ "http://localhost:1234"
  , "https://www.google.com/" -- ok
  , "asdfg" -- bad url
  , "https://www.google.com/jhgycfvuij" -- 404
  , "https://dfghjkl"
  , "https://www.google.com:1234"
  , "https://www.facebook.com"
  , "https://162.251.80.35/"
  ]

urlBandwidthTest :: [String]
urlBandwidthTest =
  [ "https://google.com"
  , "http://localhost:1234"
  ]

data CheckParameters = CheckParameters
  { checkParametersDNSTimeout :: Int
  , checkParametersHTTPTimeout :: Int
  , checkParametersTCPTimeout :: Int }

getHardcodedParameters :: CheckParameters
getHardcodedParameters = CheckParameters
  { checkParametersDNSTimeout = 1000000
  , checkParametersHTTPTimeout = 5000000
  , checkParametersTCPTimeout = 1000000 }
