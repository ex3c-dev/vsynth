module IOFunctions where
import Structures
import Control.Monad ( when, unless )
import Data.Maybe ( listToMaybe )
import Data.List ( intersperse, unfoldr)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import System.Environment ( getArgs )
import Sound.ALUT as A
import Sound.OpenAL.AL.BasicTypes (ALsizei)
import System.Process
import Text.Printf
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

outputFilePath :: FilePath
outputFilePath = "output.bin"

--save wave to format to play sound. Roll format
--take individual pulses (wave) and put it to serializable binaries in file
prepareFile :: FilePath -> [Float] -> IO ()
prepareFile filePath file = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE  file

saveAsWav :: [Pulse] -> FilePath -> IO ()
saveAsWav file inputPath = do
   prepareFile outputFilePath file
   _ <- runCommand $ printf "ffmpeg -y -f f32le -i %s -f wav %s "  outputFilePath inputPath -- ignores the result of the run command, then returns unit
   return ()

-- Function to play file
playFile :: FilePath -> IO ()
playFile fileName = do
   -- Create an AL buffer from the given sound file.
   buf <- createBuffer (File fileName)

   -- Generate a single source, attach the buffer to it and start playing.
   source <- genObjectName
   buffer source $= Just buf
   play [source]

   errs <- A.get alErrors
   unless (null errs) $ do
      hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
      exitFailure

   let waitWhilePlaying = do
          sleep 0.1
          state <- A.get (sourceState source)
          when (state == Playing) $
             waitWhilePlaying
   waitWhilePlaying


showDevice :: DeviceSpecifier -> String
showDevice Nothing = "default"
showDevice (Just d) = "'" ++ d ++ "'"

orElse :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
orElse f g = f >>= maybe g (return . Just)

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

getDeviceSpec :: String -> IO [String] -> IO DeviceSpecifier
getDeviceSpec what getter = do
   deviceSpecs <- getter
   unless (null deviceSpecs) $ do
      putStrLn $ "Found " ++ show (length deviceSpecs) ++ " " ++ what ++ ":"
      mapM_ (putStrLn . ("   " ++)) deviceSpecs
   return $ listToMaybe deviceSpecs
