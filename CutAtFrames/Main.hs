{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Turtle
import Prelude hiding (FilePath)
import Control.Error
import GHC.Int

import Prelude hiding (FilePath)

------------------------------------------------------------------------------
optsParser :: Parser (FilePath,FilePath,FilePath,Double)
optsParser = (,,,) <$> optPath "input"  'i' "Input video"
                  <*> optPath "cuts"   'c' "Input cuts file"
                  <*> optPath "output" 'o' "Output directory"
                  <*> optDouble "framerate" 'f' "Video frame rate"

------------------------------------------------------------------------------
getTimes :: FilePath -> Shell [Int]
getTimes fp =
  fmap (catMaybes . map (readMay . T.unpack) . T.lines) $ input fp

-- TODO
-- getFrameRate :: FilePath -> Shell Double
-- getFrameRate fp = do
--   res <- inshell ("ffmpeg -i " <> repr fp) empty
--   return 10

toTimestamp :: UTCTime -> Double -> Int -> T.Text
toTimestamp tNow fps nframe =
  let secs = fromIntegral nframe / fps :: Double
      t    = tNow { utctDayTime = realToFrac secs } :: UTCTime
  in  T.take 12 . T.pack $ formatTime defaultTimeLocale "%H:%M:%S.%q" t

------------------------------------------------------------------------------
main :: IO ()
main = do
  (i,c,d,fps) <- options "Break video into files" optsParser
  sh $ do
    cutFrames <- getTimes c
    undefined
  undefined
