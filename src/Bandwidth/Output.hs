module Bandwidth.Output
  ( runTests
  ) where

import Bandwidth.Check (measure)
import Bandwidth.Types (FormattedSegment (..), Measurement (..), segmentsVisualize)
import Config (urlBandwidthTest)

import Data.Text.Chart (plot)
import Data.Fixed (showFixed)
import Graphics.Gnuplot.Simple (plotFunc)
import Safe (elemIndexJust)
import Control.Monad (forM)
import Text.Printf (printf)

runTests :: IO ()
runTests = do
  results <- forM urlBandwidthTest $ \url -> do
    measurement <- measure url "any" --"wlp2s0"
    printf "Speed: %.2f kb/s\n" (measurementSpeed measurement)
    putStrLn $ "Number of RTOs: " ++ (show $ measurementRTO measurement)
    seg <- segmentsVisualize $ measurementSegments measurement
    plot seg
  return ()

showGnuplot :: Measurement -> IO ()
showGnuplot measurement = do
  let segments = measurementSegments measurement
  let ys = [(read $ showFixed True $ sSinceStart x)::Float | x <- segments]
      xs = [kbSinceStart x | x <- segments]
  plotFunc [] xs (\x -> (ys !! elemIndexJust x xs))
  _  <- getLine
  return ()
