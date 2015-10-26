{-
Cut input video into many chunks according to an input file listing
the cut positions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import           Control.Error
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time
import           Filesystem.Path.CurrentOS hiding (empty)
import           Turtle


------------------------------------------------------------------------------
optsParser :: Parser (FilePath,FilePath,FilePath,Double,Double)
optsParser = (,,,,) <$> optPath   "input"     'i' "Input video"
                    <*> optPath   "cuts"      'c' "Input cuts file"
                    <*> optPath   "output"    'o' "Output directory"
                    <*> optDouble "framerate" 'f' "Video frame rate"
                    <*> optDouble "bitesize"  'b' "Size of intermediate chunks"

------------------------------------------------------------------------------
nameLength :: Int
nameLength = 7


------------------------------------------------------------------------------
clipBites :: Double -> Double -> [Int] -> Map.Map Int (Int,Int)
clipBites fps biteSize clipFrames =
  let framesPerBite = ceiling $ biteSize * fps
      binIndex c    = c `div` framesPerBite
      clips         = zip clipFrames (tail clipFrames)
      clipGroups    = List.groupBy
                      (\(s1,_) (s2,_) -> binIndex s1 == binIndex s2) clips
      groupBite gr  = (Prelude.minimum (map fst gr),
                       Prelude.maximum (map snd gr))
      cBites        = concatMap
                      (\gr -> map (\c -> (fst c, groupBite gr)) gr)
                      clipGroups
  in  Map.fromList cBites


------------------------------------------------------------------------------
modBasename :: (Text -> Text) -> FilePath -> FilePath
modBasename fn p0 = fromText
                      (  either (const "") id (toText (directory p0))
                      <> fn (either (const "") id $ toText $ basename p0)
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
clipFiles :: Map.Map Int (Int,Int)
          -> (Int,Int)
          -> FilePath
          -> FilePath
          -> (FilePath, FilePath)
clipFiles bitesDB fs baseName outDir =
  let (bS,bE)  = fromMaybe (error $ "Bite db lookup error: " ++ show (fst fs))
                 $ Map.lookup (fst fs) bitesDB
      inFile   = biteFile (bS,bE) baseName outDir
      outFile  = outDir </> toFileName (filename baseName) fs
  in  (inFile, outFile)


------------------------------------------------------------------------------
biteFile :: (Int,Int) -> FilePath -> FilePath -> FilePath
biteFile fs baseName outDir =
  outDir </> "tmp" </> toFileName (filename baseName) fs



------------------------------------------------------------------------------
extractFrames :: Double -> (Int,Int) -> FilePath -> FilePath -> IO ()
extractFrames fps (frameStart, frameEnd) inFile outFile = do
  let cmd     = T.unwords ["ffmpeg -i"
                          ,either (const "") id (toText inFile)
                          ,"-ss " <> toTimestamp fps frameStart
                          -- ,"-c copy" -- The copy command will seek to the
                                        -- nearest keyframe. A no-no for
                                        -- short clips like ours
                          ,"-t " <> toTimestamp fps (frameEnd - frameStart)
                          ,either (const "") id (toText outFile)
                          ]
  echo cmd
  _ <- shell cmd empty
  return ()


------------------------------------------------------------------------------
toFileName :: FilePath
           -- ^ Input video file, e.g. "HomeAlone.mp4"
           -> (Int,Int)
           -- ^ Frame delimiters for this clip
           -> FilePath
toFileName movieName (_, frameEnd) =
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
  do
    cutFrames <- (0:) <$> Turtle.fold (getTimes cutFile) Fold.list
    let bitesDB = clipBites fps byteSize cutFrames

    mktree $ outDir </> "tmp"
    forM_ (List.nub $ Map.elems bitesDB) $ \(b0,bN) ->
      let outFile = biteFile (b0, bN) inFile outDir
      in  extractFrames fps (b0,bN) inFile outFile

    forM_ (zip (0:cutFrames) cutFrames) $ \fs@(cS,cE) ->
      let (clipIn, clipOut) = clipFiles bitesDB fs inFile outDir
          (bS, _)          = fromMaybe (error "Bad clip lookup")
                              (Map.lookup (fst fs) bitesDB)
      in  extractFrames fps (cS - bS, cE - bS) clipIn clipOut
  echo "Done!"


------------------------------------------------------------------------------
getTimes :: FilePath -> Shell Int
getTimes = fmap (read . T.unpack) . input
