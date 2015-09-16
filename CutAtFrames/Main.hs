{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Foldl as Fold
import Data.Foldable
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Filesystem.Path.CurrentOS hiding (empty)
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
getTimes :: FilePath -> Shell Int
getTimes fp = fmap (read . T.unpack) $ input fp
-- TODO, how to do that safely?

-- TODO
-- getFrameRate :: FilePath -> Shell Double
-- getFrameRate fp = do
--   res <- inshell ("ffmpeg -i " <> repr fp) empty
--   return 10

modBasename :: (Text -> Text) -> FilePath -> FilePath
modBasename f p0 = fromText $
                      (  either (const "") id (toText (directory p0))
                      <> f (either (const "") id $ toText $ basename p0)
                      <> "."
                      <> fromMaybe "" (extension p0))

toDir :: FilePath -> FilePath -> FilePath
toDir inFile targetDir = directory targetDir </> filename inFile

makeClip :: Int -> FilePath -> Double -> (Int,Int) -> FilePath -> IO ()
makeClip nameLength inputFile fps fs@(frameStart,frameEnd) outDir = do
  now <- getCurrentTime
  let toTS    = toTimestamp now fps
      (t,l)   = (toTS frameStart, toTS (frameEnd - frameStart))
      outName = toOutputName now nameLength fps inputFile fs outDir
      cmd     = T.unwords ["ffmpeg -i"
                          ,either (const "") id (toText inputFile)
                          ,"-ss " <> t
                          -- ,"-c copy" -- The copy command will seek to the
                                        -- nearest keyframe. A no-no for
                                        -- short clips like ours
                          ,"-t " <> l
                          ,either (const "") id (toText outName)
                          ]
  echo cmd
  shell cmd empty
  return ()

------------------------------------------------------------------------------
toTimestamp :: UTCTime -> Double -> Int -> T.Text
toTimestamp tNow fps nframe =
  let secs = fromIntegral nframe / fps :: Double
      t    = tNow { utctDayTime = realToFrac secs } :: UTCTime
  in  T.take 12 . T.pack $ formatTime defaultTimeLocale "%H:%M:%S.%q" t


------------------------------------------------------------------------------
toOutputName :: UTCTime
             -> Int
             -> Double
             -> FilePath
             -> (Int,Int)
             -> FilePath
             -> FilePath
toOutputName now nameLength fps inputFile (frameStart, frameEnd) outDir =
  let toTS    = toTimestamp now fps
      (t,l)   = (toTS frameStart, toTS (frameEnd - frameStart))
      numT    = repr frameEnd
      numStr  = T.replicate (nameLength - T.length numT) "0" <> numT
  in modBasename (<> ("_" <> numStr)) $ inputFile `toDir` outDir


------------------------------------------------------------------------------
main :: IO ()
main = do
  (i,c,d,fps) <- options "Break video into files" optsParser
  sh $ do
    cutFrames <- Turtle.fold (getTimes c) Fold.list
    forM_ (zip (0:cutFrames) cutFrames) $ \fs ->
      liftIO $ makeClip 6 i fps fs d
  echo "Done!"
