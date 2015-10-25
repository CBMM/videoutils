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

nameLength :: Int
nameLength = 6


-- type F = ((Int,Int), [Int], [(Int,(Int,Int))])
-- clipBites :: Double -> Double -> [Int] -> Map.Map Int (Int,Int)
-- clipBites fps biteSize clipFrames =
--   let framesPerBite = ceiling $ biteSize * fps
--       state0 = ((0,0), [], [])
--       aux :: F ->  Int -> F
--       aux ((biteFrame0,biteFrameN), accClips, accAssocList) c
--         | c - biteFrame0 < framesPerBite =
--           ((biteFrame0, c), c:accClips, accAssocList)
--         | c - biteFrame0 >= framesPerBite =
--           let newEntries = map (,(biteFrame0,biteFrameN)) accClips
--           in  ((c,c), [], newEntries ++ accAssocList)
--       getMap (_,_,m) = m
--   in  Map.fromList . reverse . getMap $ (foldl' aux state0 clipFrames :: F)

clipBites :: Double -> Double -> [Int] -> Map.Map Int (Int,Int)
clipBites fps biteSize clipFrames =
  let framesPerBite = ceiling $ biteSize * fps
      binIndex c    = c `div` framesPerBite
      clips         = zip clipFrames (tail clipFrames)
      clipGroups    = List.groupBy
                      (\(s1,e1) (s2,e2) -> binIndex s1 == binIndex s2) clips
      groupBite g   = (Prelude.minimum (map fst g), Prelude.maximum (map snd g))
      clipBites     = Prelude.concat $
                      map (\g -> map (\c -> (fst c, groupBite g)) g)
                      clipGroups
  in  Map.fromList $ clipBites


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
-- | Filename for given video clip
clipFiles :: Double
          -> Map.Map Int (Int,Int)
          -> (Int,Int)
          -> FilePath
          -> FilePath
          -> (FilePath, FilePath)
clipFiles fps bitesDB fs baseName outDir =
  let (bS,bE)  = fromMaybe (error $ "Bite db lookup error: " ++ show (fst fs))
                 $ Map.lookup (fst fs) bitesDB
      inFile   = biteFile fps (bS,bE) baseName outDir
      outFile  = outDir </> toFileName fps (filename baseName) fs
  in  (inFile, outFile)

biteFile :: Double -> (Int,Int) -> FilePath -> FilePath -> FilePath
biteFile fps fs baseName outDir =
  outDir </> "tmp" </> toFileName fps (filename baseName) fs

extractFrames :: Double -> (Int,Int) -> FilePath -> FilePath -> IO ()
extractFrames fps (frameStart, frameEnd) inFile outFile = do
  let cmd     = T.unwords ["ffmpeg -i"
                          ,either (const "") id (toText inFile)
                          ,"-ss " <> (toTimestamp fps frameStart)
                          -- ,"-c copy" -- The copy command will seek to the
                                        -- nearest keyframe. A no-no for
                                        -- short clips like ours
                          ,"-t " <> (toTimestamp fps frameEnd)
                          ,either (const "") id (toText outFile)
                          ]
  echo cmd
  print $ unwords ["inFile:", show inFile, "outName:", show outFile]
  shell cmd empty
  return ()

------------------------------------------------------------------------------
toFileName :: Double
           -- ^ Frames per second of base video
           -> FilePath
           -- ^ Input video file, e.g. "HomeAlone.mp4"
           -> (Int,Int)
           -- ^ Frame delimiters for this clip
           -> FilePath
toFileName fps movieName (frameStart, frameEnd) =
  let numT    = repr frameEnd
      numStr  = T.replicate (nameLength - T.length numT) "0" <> numT
  in filename $ modBasename (<> ("_" <> numStr)) movieName


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
  do
    cutFrames <- (0:) <$> Turtle.fold (getTimes cutFile) Fold.list
    let bitesDB = clipBites fps byteSize cutFrames

    mktree $ outDir </> "tmp"
    forM_ (List.nub $ Map.elems bitesDB) $ \(b0,bN) ->
      let outFile = biteFile fps (b0, bN) inFile outDir
      in  extractFrames fps (b0,bN) inFile outFile

    forM_ (zip (0:cutFrames) cutFrames) $ \fs ->
      let (clipIn, clipOut) = clipFiles fps bitesDB fs inFile outDir
      in  extractFrames fps fs clipIn clipOut
  echo "Done!"


tmpIn :: FilePath -> FilePath
tmpIn = (</> "tmp")

------------------------------------------------------------------------------
getTimes :: FilePath -> Shell Int
getTimes fp = fmap (read . T.unpack) $ input fp

-- TODO testing stuff - delete
test :: IO ([Int], Map.Map Int (Int,Int))
test = do
  cFrames <- Turtle.fold (getTimes "/Users/greghale/Documents/Video/HomeAlone2/cuts.txt") Fold.list
  return (0:cFrames, clipBites 23.978 20 (0:cFrames))

rawVid = "/Users/greghale/Documents/Video/HomeAlone2/Home_Alone_2_PG.mp4" :: FilePath
oDir   = "/Users/greghale/Documents/Video/HomeAlone2/testmp4/tmp" :: FilePath
