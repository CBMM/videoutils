{-
Cut input video into many chunks according to an input file listing
the cut positions.

The naive way, run ffmpeg over the full input for each requested chunk,
is O(n^2) in the video length. So, chop the original into several chunks,
and do the lookup from those.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import qualified Control.Foldl as Fold
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Filesystem.Path.CurrentOS hiding (empty)
import Turtle
import Control.Error
import GHC.Int

import Prelude hiding (FilePath)

------------------------------------------------------------------------------
optsParser :: Parser (FilePath,FilePath,FilePath,Double,Double)
optsParser = (,,,,) <$> optPath   "input"     'i' "Input video"
                    <*> optPath   "cuts"      'c' "Input cuts file"
                    <*> optPath   "output"    'o' "Output directory"
                    <*> optDouble "framerate" 'f' "Video frame rate"
                    <*> optDouble "bitesize"  'b' "Size of intermediate chunks"

------------------------------------------------------------------------------
getTimes :: FilePath -> Shell Int
getTimes fp = fmap (read . T.unpack) $ input fp
-- TODO, how to do that safely?

-- TODO
-- getFrameRate :: FilePath -> Shell Double
-- getFrameRate fp = do
--   res <- inshell ("ffmpeg -i " <> repr fp) empty
--   return 10

type F = ((Int,Int), [Int], [(Int,(Int,Int))])
clipBites :: Double -> Double -> [Int] -> Map.Map Int (Int,Int)
clipBites fps biteSize clipFrames =
  let framesPerBite = ceiling $ biteSize * fps
      state0 = ((0,0), [], [])
      aux :: F ->  Int -> F
      aux ((biteFrame0,biteFrameN), accClips, accAssocList) c
        | c - biteFrame0 < framesPerBite =
          ((biteFrame0, c), c:accClips, accAssocList)
        | c - biteFrame0 >= framesPerBite =
          let newEntries = map (,(biteFrame0,biteFrameN)) accClips
          in  ((c,c), [], newEntries ++ accAssocList)
      getMap (_,_,m) = m
  in  Map.fromList . reverse . getMap $ (foldl' aux state0 clipFrames :: F)


modBasename :: (Text -> Text) -> FilePath -> FilePath
modBasename f p0 = fromText
                      (  either (const "") id (toText (directory p0))
                      <> f (either (const "") id $ toText $ basename p0)
                      <> "."
                      <> fromMaybe "" (extension p0))


-------------------------------------------------------------------------------
-- | The filename of the first argument, put in the directory of
--   the second argument
toDir :: FilePath -- ^ File to use as filename
      -> FilePath -- ^ File to use as path
      -> FilePath -- ^ File at Path
toDir inFile targetDir = directory targetDir </> filename inFile


-------------------------------------------------------------------------------
-- | Make a movie clip by looking in $baseDir/tmp for a big fragment
--   containing these frames
makeClip :: Int
            -- ^ Length to pad frame index to, (6 for < 1e6 frames)
         -> Map.Map Int (Int,Int)
         -- ^ Clip to bite mapping
         -> Double
         -- ^ Frames per second of base movie
         -> (Int,Int)
         -- ^ First and last frames in this clip
         -> FilePath
         -- ^ $baseDir
         -> IO ()
makeClip nameLength biteDB fps fs@(frameStart,frameEnd) baseDir = do
  let toTS    = toTimestamp fps

      (bT,bL) = fromMaybe (error "Bite DB lookup error") $
                Map.lookup frameStart biteDB

      -- Filename of the bite to use ( (100,200) -> )
      inFile  = toFileName nameLength fps baseDir (bT,bL)
                (baseDir </> "tmp")
      (t,l)   = (toTS frameStart, toTS (frameEnd - frameStart))
      outName = toFileName nameLength fps inFile fs baseDir

      cmd     = T.unwords ["ffmpeg -i"
                          ,either (const "") id (toText inFile)
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


makeBite :: Int -> FilePath -> Double -> (Int,Int) -> FilePath -> IO ()
makeBite nameLength inputFile fps fs@(frameStart,frameEnd) outDir = do
  let toTS    = toTimestamp fps
      (t,l)   = (toTS frameStart, toTS (frameEnd - frameStart))
      outName = toFileName nameLength fps inputFile fs
                (outDir <> "tmp/")
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
toFileName :: Int
           -- ^ Name to pad index to in filename. 6 -> 3 -> "000003"
           -> Double
           -- ^ Frames per second of base video
           -> FilePath
           -- ^ Input video file, just used for its name root, e.g. "HomeAlone"
           -> (Int,Int)
           -- ^ Frame delimiters for this clip
           -> FilePath
           -- ^ Base directory for saving all clips
           -> FilePath
toFileName nameLength fps inputFile (frameStart, frameEnd) baseDir =
  let numT    = repr frameEnd
      numStr  = T.replicate (nameLength - T.length numT) "0" <> numT
  in modBasename (<> ("_" <> numStr)) $ inputFile `toDir` baseDir


------------------------------------------------------------------------------
toTimestamp :: Double -> Int -> T.Text
toTimestamp fps nframe =
  let secs = fromIntegral nframe / fps :: Double
      tBase = UTCTime (fromGregorian 2015 1 1) 0
      t    = tBase { utctDayTime = realToFrac secs } :: UTCTime
  in  T.take 12 . T.pack $ formatTime defaultTimeLocale "%H:%M:%S.%q" t

------------------------------------------------------------------------------
main :: IO ()
main = do
  (inFile,cutFile,outDir,fps,byteSize) <- options "Break video into files" optsParser
  let nameLen = 6
  sh $ do
    cutFrames <- Turtle.fold (getTimes cutFile) Fold.list
    let bitesDB = clipBites fps byteSize cutFrames

    mkdir $ outDir </> "tmp"
    forM_ (List.nub $ Map.elems bitesDB) $ \(b0,bN) ->
      liftIO $ makeBite nameLen inFile fps (b0,bN)
               (toFileName nameLen fps inFile (b0,bN) (outDir </> "tmp"))

    forM_ (zip (0:cutFrames) cutFrames) $ \fs ->
      liftIO $ makeClip 6 bitesDB outDir fps fs (toFileName 6 fps outDir fs outDir)
  echo "Done!"
