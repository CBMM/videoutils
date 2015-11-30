{-
Save frames out to one tmp directory, copy the needed ones to a second tmp
directory, reassemble a movie at the desired framerate from the second tmp
frames.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import           Control.Error
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time
import           Filesystem.Path.CurrentOS hiding (empty, addExtension)
import           System.Directory (copyFile, doesDirectoryExist, getDirectoryContents, 
                                   removeDirectoryRecursive)
import           System.FilePath.Posix hiding (FilePath, (</>))
import           Turtle hiding (FilePath, (</>))


------------------------------------------------------------------------------
main :: IO ()
main = do
  opts <- options "Rescale video framerate" optsParser
  let makeClean (d :: FilePath)  = do
        b <- doesDirectoryExist (encodeString d)
        when b $ removeDirectoryRecursive (encodeString d) 
        mktree (d)
      inTmp       = tmpDir opts </> "inframes"
      outTmp      = tmpDir opts </> "outframes"
  makeClean (inTmp)
  makeClean (outTmp)
  touch (quietFile opts)
  rm (quietFile opts)
  touch (tmpDir opts </> "sound.wav")
  rm (tmpDir opts </> "sound.wav")
  vidToFrames opts
  inFrms  <- filter ((==".png") . takeExtension) <$> getDirectoryContents (encodeString inTmp)
  let outFrms = filter (> 0) $ outputFrameNums opts (length inFrms)
  forM_ (zip outFrms [0..]) $ \(iIn, iOut) -> 
    copyFile (encodeString $ inTmp </> toFileName iIn) (encodeString $ outTmp </> toFileName iOut)
  framesToVid opts 
  bringAudio opts
  print "Done!"

bringAudio opts@Opts{..} =
  let soundFile = tmpDir </> "sound.wav"
      ripCmd = T.unwords ["ffmpeg"
                         ,"-i", either (error . T.unpack) id (toText inFile)
                         , T.pack (encodeString soundFile)
                         ]
      mixCmd = T.unwords ["ffmpeg"
                         ,"-i", T.pack (encodeString $ quietFile opts)
                         ,"-i", T.pack (encodeString soundFile)
                         ,"-q:v","31"
                         , either (error . T.unpack) id (toText outFile)
                         ]
  in  print ripCmd >> shell ripCmd empty >> print mixCmd >> shell mixCmd empty >> return ()


quietFile :: Opts -> FilePath
quietFile Opts{..} = tmpDir </> (decode . T.pack $ addExtension "quiet" (takeExtension $ encodeString outFile))


------------------------------------------------------------------------------
data Opts = Opts 
  { inFile  :: FilePath
  , outFile :: FilePath
  , inFPS   :: Double
  , outFPS  :: Double
  , tmpDir  :: FilePath
  } deriving (Show)


------------------------------------------------------------------------------
outputFrameNums :: Opts -> Int -> [Int]
outputFrameNums Opts{..} nInputFrames =
  let nOutputFrames = floor (realToFrac nInputFrames * outFPS / inFPS)
      outputTimes   = map ((/ outFPS) . realToFrac) [1..nOutputFrames-1]
      outputFrames  = map (round . (* inFPS)) outputTimes
  in  outputFrames
  

------------------------------------------------------------------------------
optsParser :: Parser Opts
optsParser = Opts <$> optPath   "input"     'i' "Input video"
                  <*> optPath   "output"    'o' "Output video"
                  <*> optDouble "infps"     'f' "Input FPS"
                  <*> optDouble "outfps"    'g' "Output FPS"
                  <*> optPath   "tmpdir"    't' "Path for temp files"

------------------------------------------------------------------------------
nameLength :: Int
nameLength = 7


------------------------------------------------------------------------------
vidToFrames :: Opts -> IO ()
vidToFrames Opts{..} = 
  let fnm = "image%0" <> showt nameLength <> "d.png"
      cmd = T.unwords ["ffmpeg"
                      ,"-i", either (error . T.unpack) id (toText inFile)
                      ,"-r", showt inFPS
                      ,"-f","image2"
                      ,either (error . T.unpack) id 
                        (toText $ tmpDir </> "inframes" </> fromText fnm)
                      ]
  in  do
        shell cmd empty >> return ()


------------------------------------------------------------------------------
framesToVid :: Opts -> IO ()
framesToVid opts@Opts{..} =
  let fnm = "image%0" <> showt nameLength <> "d.png"
      cmd = T.unwords ["ffmpeg"
                      ,"-start_number","1"
                      ,"-framerate",showt outFPS
                      ,"-i", either (error . T.unpack) id 
                       (toText $ tmpDir </> "outframes" </> fromText fnm)
                      -- ,"-c:v","libx264"
                      ,"-q:v","31"
                      ,"-r", showt outFPS
                      ,"-pix_fmt", "yuv420p"
                      ,either (error . T.unpack) id (toText $quietFile opts)
                      ]
  in shell cmd empty >> print ("COMMAND IS: " <> cmd)

showt :: (Show a) => a -> T.Text
showt = T.pack . show


------------------------------------------------------------------------------
modBasename :: (Text -> Text) -> FilePath -> FilePath
modBasename fn p0 = fromText
                      (  either (const "") id (toText (directory p0))
                      <> fn (either (const "") id $ toText $ basename p0)
                      <> "."
                      <> fromMaybe "" (extension p0))


------------------------------------------------------------------------------
toFileName :: Int -- ^ Frame number
           -> FilePath
toFileName n =
  let numStr  = T.replicate (nameLength - T.length (showt n)) "0" <> showt n
  in  fromText $ "image" <> numStr <> ".png"



