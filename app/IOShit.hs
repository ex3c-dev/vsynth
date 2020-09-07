module IOShit where
import Structures
import Control.Concurrent ( threadDelay )
import Control.Monad ( when, unless )
import Data.Maybe ( listToMaybe )
import Data.List ( intersperse, zipWith3, unfoldr)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Unsafe as B
import Data.Foldable
import Data.WAVE
import System.Environment ( getArgs )
import Sound.ALUT as A
import Sound.OpenAL.AL.BasicTypes (ALsizei)
import System.Process
import Text.Printf
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Data.IORef

outputFilePath :: FilePath
outputFilePath = "output.bin"
inputPath :: FilePath
inputPath = "fuckyou.wav"

--save wave to format to play sound. Roll format
--take individual pulses (wave) and put it to serializable binaries in file

prepareFile :: FilePath -> [Float] -> IO ()
prepareFile filePath file = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE  file

saveAsWav :: [Pulse] -> IO ()
saveAsWav file = do
   prepareFile outputFilePath file
   _ <- runCommand $ printf "ffmpeg -y -f f32le -i %s -f wav %s "  outputFilePath inputPath -- ignores the result of the run command, then returns unit
   return ()

-- This program loads and plays a variety of files.
playFile :: FilePath -> IO ()
playFile fileName = do
   -- Create an AL buffer from the given sound file.
   buf <- createBuffer (File fileName)
   --buf2 <- createBuffer $ FileImage $ memoryRegion $ test2

   -- Generate a single source, attach the buffer to it and start playing.
   source <- genObjectName
   buffer source $= Just buf
   --[source] <- genObjectNames 1
   --queueBuffers source [buf,buf2]

   play [source]

   -- Normally nothing should go wrong above, but one never knows...
   errs <- A.get alErrors
   unless (null errs) $ do
      hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
      exitFailure

   -- Check every 0.1 seconds if the sound is still playing.
   let waitWhilePlaying = do
          sleep 0.1
          state <- A.get (sourceState source)
          when (state == Playing) $
             waitWhilePlaying
   waitWhilePlaying




-- not working lel

