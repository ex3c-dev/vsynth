module Main where
import Structures
import Utils
import IOShit
import Lib
import Music
import Functions
import Control.Concurrent ( threadDelay )
import Control.Monad ( when, unless, liftM )
import Data.Maybe ( listToMaybe )
import Data.List ( intersperse, zipWith3, unfoldr)
import Data.Foldable
import Data.Word (Word8)
import Data.WAVE
import Data.Int (Int32)
import System.Environment ( getArgs )
import Sound.ALUT as A
import Sound.OpenAL.AL.BasicTypes (ALsizei)
import System.Process
import Text.Printf
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent
import Data.IORef
import Gui




main :: IO ()
main = do

    withProgNameAndArgs runALUT $ \progName args -> do

        unless (length args == 1) $ do
            hPutStrLn stderr ("usage: " ++ progName ++ " <fileName>")
            exitFailure
        playFile (head args)

        createSheet axisOfAwesome A octave 4 4
