module Bandwidth.Types
  ( Segment (..)
  , FormattedSegment (..)
  , Measurement (..)
  , format
  , segmentsVisualize
  ) where

import Data.Time.Clock (UTCTime, nominalDiffTimeToSeconds, diffUTCTime)
import Data.Fixed (Pico, showFixed)
import GHC.Float (int2Float)
import GHC.Float.RealFracMethods (truncateFloatInteger)
import Control.Monad (forM)
import qualified System.Console.Terminal.Size as Size
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import Data.Word (Word16)

data Measurement = Measurement
  { measurementSegments :: [FormattedSegment]
  , measurementSpeed :: Float
  , measurementPacketLoss :: Maybe Int
  , measurementRTO :: Word16 }

data Segment = Segment
  { bytes :: Int
  , time :: UTCTime }

data FormattedSegment = FormattedSegment
  { kbSinceStart :: Float
  , sSinceStart :: Pico }

format :: [Segment] -> [FormattedSegment]
format [] = []
format segments = fn segments [FormattedSegment 0 0]
  where
    fn :: [Segment] -> [FormattedSegment] -> [FormattedSegment]
    fn (x:[]) res = res
    fn (first:second:left) res = fn (first : left) (res ++ [FormattedSegment (kbDownloaded (kbSinceStart $ last res) (bytes second)) (timeBetweenMs (time first) (time second))])

kbDownloaded :: Float -> Int -> Float
kbDownloaded prevKb curB = prevKb + ((int2Float curB) / 1024)

timeBetweenMs :: UTCTime -> UTCTime -> Pico
timeBetweenMs f s = nominalDiffTimeToSeconds $ diffUTCTime s f

secondsToMsFloat :: Pico -> Float
secondsToMsFloat = (read . (showFixed True)) . (*1000)

instance Show FormattedSegment where
  show seg = "(" ++ (show $ kbSinceStart seg) ++ "kb, " ++ (show $ secondsToMsFloat $ sSinceStart seg) ++ "ms)"

-- formatting for asciichart
segmentsVisualize :: [FormattedSegment] -> IO [Integer]
segmentsVisualize segments = do
  let timesForKbs = map snd (hlp (tail segments) [])
  let diff = ceiling (int2Float (length timesForKbs) / fromInteger (outputWidth - 10))
  return $ every diff timesForKbs
  where
    outputWidth :: Integer
    outputWidth = fromMaybe 120 (fmap Size.width (unsafePerformIO Size.size))

    every :: Int -> [a] -> [a]
    every n xs = case drop (n-1) xs of
              y : ys -> y : every n ys
              [] -> []

    hlp :: [FormattedSegment] -> [(Integer, Integer)] -> [(Integer, Integer)]
    hlp [] out  = out
    hlp inp []  = hlp inp [(0, 0)]
    hlp inp out = case (truncateFloatInteger $ kbSinceStart cur) - (fst prevRes) of
                0 -> hlp (tail inp) (out ++ [(fst prevRes, segmentSeconds cur)])
                1 -> hlp (tail inp) (out ++ [((fst prevRes) + 1, segmentSeconds cur)])
                otherwise -> hlp (inp) (out ++ [((fst prevRes) + 1, snd prevRes)])
      where
        prevRes = last out
        cur = head inp
        segmentSeconds = truncateFloatInteger . secondsToMsFloat . sSinceStart
