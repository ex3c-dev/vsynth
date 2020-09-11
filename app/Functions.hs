{-# LANGUAGE TemplateHaskell #-}
module Functions where
import Control.Lens

import Structures

makeKey :: [Semitone] -> Semitone -> Octave -> [Pulse]
makeKey line key octave = [x + key + (octave * 12)| x <- line]

--Function that shifts (root) note to another key 
makeKey2 :: [Semitone] -> Key -> Octave -> [Pulse]
makeKey2 line k octave = map (\x -> x + key(k) + (octave * 12)) line

createScale2 :: Key -> [Semitone] -> Octave -> Scale Semitone
createScale2 root intervals octave = (Scale (map (\x -> x + key(root) + (octave * 12)) intervals))

--Creates Scale in Key in Octave
createScale :: Key -> [Semitone] -> Octave -> Scale Semitone
createScale root intervals octave = (Scale (makeKey2 intervals root octave))

--transposes Scale to another key
transposeScale :: Num a => Scale a -> a -> Scale a
transposeScale scale i = (Scale (map (\x -> x + i) (intervals scale)))

--actually functional transpose function lmao. please replace the old one 
-- basically subtract the chord we want to go to from the current chord to get an offset.
transposeScale2 :: Scale Semitone -> Semitone -> Key-> Scale Semitone
transposeScale2 scale i k = 
    let offset = i - (key(k))
    in (Scale (map (\x -> x + offset) (intervals scale)))


--Create some basic scales
majorScale root octave = createScale root majScale octave

minorScale root octave = createScale root minScale octave

majorPentagonic root octave = createScale root majPentagonic octave

minorPentagonic root octave = createScale root minPentagonic octave

diminishedScale root octave = createScale root dimScale octave

augmentedScale root octave = createScale root augScale octave

chordScale root octave = createScale root fifthsMScale octave

fifthsMinorScale root octave = createScale root fifthsmScale octave


--  Fetches Enum value by indice
getKey :: Int -> Key
getKey i = [A ..] !! (i `mod` 12)

-- Fetches indice from Key
key :: Enum a => a -> Semitone
key k = fromIntegral $ fromEnum k 

-- returns key from a chord progression indice
mapProgToKey :: Key -> Int -> Octave -> Semitone
mapProgToKey k indice octave =
    let scale = majorScale k octave
    in (intervals scale) !! (indice -1)

mapProgToKey2 :: Key -> Int -> Octave -> Semitone
mapProgToKey2 k indice octave =
    let scale = majorScale k octave
    in ((intervals scale) !! (indice -1)) -1

