{-
Cut input video into many chunks according to an input file listing
the cut positions.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Prelude hiding (FilePath)
import           Control.Error
import qualified Data.ByteString.Char8    as BS
import           Data.Foldable
import           Data.Function            (on)
import qualified Data.List as List
import qualified Data.Text as T
import           Data.Time
import           Filesystem.Path.CurrentOS hiding (empty)
import           Turtle


------------------------------------------------------------------------------
data Opts = Opts
  { oInput     :: FilePath
  , oCuts      :: FilePath
  , oOutputDir :: FilePath
  , oFPS       :: Double
  , oBiteSize  :: Double
  } deriving (Show)


------------------------------------------------------------------------------
clipBites :: Double -> Double -> [(Int,Int)] -> [((Int,Int), (Int,Int))]
clipBites fps biteSize clips =
  let framesPerBite  = floor $ biteSize * fps :: Int
      binIndex (c,_)   = c `div` framesPerBite
      clipGroups       = List.groupBy ((==) `on` binIndex) clips
      groupBite gr     = (Prelude.minimum (map fst gr),
                          Prelude.maximum (map snd gr))
      groupsAndBites   = zip clipGroups (map groupBite clipGroups)
      cBites           = concatMap (\(cs, gr) -> map (,gr) cs) groupsAndBites
  in  cBites


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
clipFiles :: ((Int,Int), (Int,Int))
          -> FilePath
          -> FilePath
          -> (FilePath, FilePath)
clipFiles (fs, (bS,bE)) baseName outDir =
  let inFile   = biteFile (bS,bE) baseName outDir
      outFile  = outDir </> toFileName (filename baseName) fs
  in  (inFile, outFile)


------------------------------------------------------------------------------
biteFile :: (Int,Int) -> FilePath -> FilePath -> FilePath
biteFile fs baseName outDir =
  outDir </> "tmp" </> toFileName (filename baseName) fs



------------------------------------------------------------------------------
extractFrames :: Double -> (Int,Int) -> FilePath -> FilePath -> IO ()
extractFrames fps (frameStart, frameEnd) inFile outFile = do
  let cmd     = T.unwords ["ffmpeg -v 1 -i"
                          ,either (const "") id (toText inFile)
                          ,"-ss " <> toTimestamp fps frameStart
                          -- ,"-c copy" -- The copy command will seek to the
                                        -- nearest keyframe. A no-no for
                                        -- short clips like ours
                          ,"-t " <> toTimestamp fps (frameEnd - frameStart)
                          ,"-strict -2 -b:v 2M"
                          ,either (const "") id (toText outFile)
                          ]
  echo cmd
  _ <- shell cmd empty
  return ()


-------------------------------------------------------------------------------
toFileName :: FilePath
           -- ^ Input video file, e.g. "HomeAlone.mp4"
           -> (Int,Int)
           -- ^ Frame delimiters for this clip
           -> FilePath
toFileName movieName (_, frameEnd) =
  let numT    = repr frameEnd
      numStr  = T.replicate (nameLength - T.length numT) "0" <> numT
  in filename $ modBasename (<> ("_" <> numStr)) movieName



-------------------------------------------------------------------------------
toTimestamp :: Double -> Int -> T.Text
toTimestamp fps nframe =
  let secs = fromIntegral nframe / fps :: Double
      tBase = UTCTime (fromGregorian 2015 1 1) 0
      t    = tBase { utctDayTime = realToFrac secs } :: UTCTime
  in  T.take 12 . T.pack $ formatTime defaultTimeLocale "%H:%M:%S.%q" t


-------------------------------------------------------------------------------
main :: IO ()
main = do
  op@Opts{..} <- options "Break video into files" optsParser
  validateOpts op
  do
    cutFrames <- (0:) <$> getCutFrames oCuts
    let clipBounds = zip cutFrames (tail cutFrames)
        bitesDB = clipBites oFPS oBiteSize clipBounds :: [((Int,Int), (Int,Int))]

    mktree oOutputDir
    mkdir $ oOutputDir </> "tmp"

    forM_ (List.nub $ map snd bitesDB) $ \(b0,bN) -> do
      let outFile = biteFile (b0, bN) oInput oOutputDir
      putStrLn $ unwords ["Processing byte:", show (b0,bN)]
      extractFrames oFPS (b0,bN) oInput outFile

    forM_ bitesDB $ \((cS,cE),(bS,bE)) -> do
      let (clipIn, clipOut) = clipFiles ((cS,cE),(bS,bE)) oInput oOutputDir
      putStrLn $ unwords ["Processing clip", show clipIn, "from bite", show (bS,bE)]
      extractFrames oFPS (cS - bS + 1, cE - bS) clipIn clipOut -- TODO the +1 here is a hack
  echo "Done!"


-------------------------------------------------------------------------------
validateOpts :: Opts -> IO ()
validateOpts Opts{..} = do
  cuts <- getCutFrames oCuts
  let lengths = zipWith (-) (tail cuts) cuts
  when (List.sort cuts /= cuts)
    (error "Frames in cut file should be sorted")
  when (any (< 1) lengths)
    (error "All clips should be at least 1 frame long")
  return ()

-------------------------------------------------------------------------------
getCutFrames :: FilePath -> IO [Int]
getCutFrames cuts =
  map (read . BS.unpack) . BS.lines <$> BS.readFile (encodeString cuts)


------------------------------------------------------------------------------
optsParser :: Parser Opts
optsParser = Opts <$> optPath   "input"     'i' "Input video"
                  <*> optPath   "cuts"      'c' "Input cuts file"
                  <*> optPath   "output"    'o' "Output directory"
                  <*> optDouble "framerate" 'f' "Video frame rate"
                  <*> optDouble "bitesize"  'b' "Size of intermediate chunks"


------------------------------------------------------------------------------
nameLength :: Int
nameLength = 8


------------------------------------------------------------------------------
testOpts :: Opts
testOpts = Opts
           "/Users/greghale/Documents/Video/HomeAlone2/Home_Alone_2_PG.mp4"
           "/Users/greghale/Documents/Video/HomeAlone2/cuts.txt"
           "/Users/greghale/Documents/Video/HomeAlone2/try3"
           23.978
           120
