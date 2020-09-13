module Functions where

import Structures
import Settings

--  Fetches Key value by indice
getKey :: Int -> Key
getKey i = [A ..] !! (i `mod` 12)

-- Fetches Enum indice from Key
key :: Enum a => a -> Semitone
key k = fromIntegral $ fromEnum k 

--Function that shifts (root) note to another key 
makeKey :: [Semitone] -> Key -> Octave -> [Pulse]
makeKey line k octave = map (\x -> x + key(k) + (octave * 12)) line

--Creates Scale in Key in Octave
createScale :: Key -> [Interval] -> Octave -> Scale Semitone
createScale root intervals octave = Scale $ makeKey intervals root octave

makeScale :: Scale Semitone -> Semitone -> Scale Semitone
makeScale scale i = Scale $ map (\x -> x + i) (intervals scale)

--subtract the chord we want to go to from the current chord to get an offset.
transposeScale2 :: Scale Semitone -> Semitone -> Key-> Scale Semitone
transposeScale2 scale i k = Scale $ map (\x -> x + (i - key k )) (intervals scale)

-- returns key from a chord progression indice
mapProgToKey2 :: Key -> Int -> Octave -> Semitone
mapProgToKey2 k indice octave =
    let scale = majorScale k octave
    in ((intervals scale) !! (indice -1)) -1

--Create some basic scales
majorScale root octave = createScale root majScale octave

minorScale root octave = createScale root minScale octave

majorPentagonic root octave = createScale root majPentagonic octave

minorPentagonic root octave = createScale root minPentagonic octave

diminishedScale root octave = createScale root dimScale octave

augmentedScale root octave = createScale root augScale octave

chordScale root octave = createScale root fifthsMScale octave

fifthsMinorScale root octave = createScale root fifthsmScale octave


f :: Semitone -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Note -> [Pulse]
note n = map (* _vol n) $ freq (f (_semitone n)) (_dur n * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = zipWith3 (\x y z -> x * y * z) release attack output
    where
      step :: Hz
      step = (hz * 2 * pi) / sampleRate  --A440 pitch standard
      attack :: [Pulse]
      attack = map (min 1.0) [0.0, 0.001 ..]
      release :: [Pulse]
      release = reverse $ take (length output) attack
      output :: [Pulse]
      output = map sin $ map (*step) [ 0.0 .. sampleRate * duration]

makeLine :: [Note] -> [Pulse]
makeLine notes  = concat [note n | n <- notes]

-- whatever this kind of 3 times list comprehension is called
makeNote :: [Semitone] -> [Beats] -> [Volume] -> [Note]
makeNote line dur vol = [Note x y z |(x, y, z) <- zip3 line dur vol]
