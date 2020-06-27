module Main where

import Lib
import Sound.Pulse.Simple
-- Library collides with many standart function so import it as qualified within namespace B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable

main :: IO ()
main = do
    s <- simpleNew Nothing "example" Record Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    xs <- simpleRead s $ 44100*10 :: IO [Float]
    simpleFree s
    --print xs
    B.writeFile "output.bin" $ B.toLazyByteString $ fold $ map B.floatLE xs

volume :: Float
volume = 0.5

-- Create sequence of floats to descripe a sound wave
wave ::[Float]
wave = map (* volume) $ map sin $ map (* step) [0.0 .. 48000]
    where step = 0.01

save :: IO ()
save = B.writeFile "output.bin" $ B.toLazyByteString $ fold $ map B.floatLE wave
