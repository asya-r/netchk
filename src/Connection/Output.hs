{-# LANGUAGE OverloadedStrings #-}
module Connection.Output
  ( runTests
  ) where

import Connection.Check (checkConnection)
import Connection.Types (showRes, Result)
import Config (getHardcodedUrls)

import Control.Monad (forM)
import System.IO
import System.Console.Pretty (color, Color (..))
import System.Console.ANSI (clearFromCursorToLineBeginning, setCursorColumn)

runTests :: IO ()
runTests = do
  hSetBuffering stdout NoBuffering
  results <- forM getHardcodedUrls $ \url -> showResult url
  return ()

showResult :: String -> IO Result
showResult url = do
  putStr $ (color White "⬤") ++ " \"" ++ url ++ "\"......."
  res <- checkConnection url
  clearFromCursorToLineBeginning
  setCursorColumn 0
  case res of
    Left _ -> putStrLn $ (color Red "⬤") ++ " \"" ++ url ++ "\": Fail (" ++ (showRes res) ++ ")"
    Right _ -> putStrLn $ (color Green "⬤") ++ " \"" ++ url ++ "\": " ++ (showRes res)
  return res
