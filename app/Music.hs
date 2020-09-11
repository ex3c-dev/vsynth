{-# LANGUAGE TemplateHaskell #-}

module Music where 

import Structures
import Utils
import IOShit
import Lib
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

volume :: Volume
volume = 0.05

octave :: Octave
octave = 0.0

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration =  60.0 / bpm

beat :: Float -> Beats
beat n = beatDuration * n

qn :: Beats
qn = 1

f :: Semitone -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Note -> [Pulse]
note n = map (* _vol n) $ freq (f (_semitone n)) (_dur n * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = zipWith3 (\x y z -> x * y * z) release attack output
   where
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

arpeggio:: Semitone -> Scale Semitone -> [Beats] -> [Volume] -> [Pulse]
arpeggio key tscale dur vol = 
    let scale = transposeScale tscale key
        arpeg = chord ++ [0]
        chordDone = [(intervals scale) !! x | x <- arpeg]
        notes = makeNote chordDone dur vol
    in makeLine notes

---try and do this with list comprehension with i in length (chord)
triad :: Semitone -> Scale Semitone -> Beats -> Volume -> [Pulse]
triad key tscale dur vol = 
    let scale = transposeScale tscale key
        v = take (length chord) $ repeat vol
        d = take (length chord) $ repeat dur
        chordDone = [(intervals scale) !! x | x <- chord]
        notes = makeNote chordDone d v
    in concat [zipWith3 (\x y z -> x + y + z) (makeLine ([notes !! 0])) (makeLine ([notes !! 1])) (makeLine ([notes !! 2]))]

chordInMinorProg :: Int -> Key -> Octave -> [Pulse]
chordInMinorProg index k octave 
    | index == 1 || index == 5  || index == 6 = triad (i) (majorScale A octave) qn volume
    | index == 4 = triad (i) (minorScale A octave) qn volume 
    | index == 2 || index == 7 = triad (i) (diminishedScale A octave) qn volume
    | index == 3 = triad (i) (augmentedScale A octave) qn volume
    | otherwise =  error "Invalid chord progression"
    where i = mapProgToKey2 k index octave

--scope bindings over several guarded equations
chordInMajorProgression :: Int -> Key -> Octave -> [Pulse]
chordInMajorProgression index k octave 
    | index == 1 || index == 4 || index == 5 = triad (i) (majorScale A octave) qn volume
    | index == 2 || index == 3 || index == 6 = triad (i) (minorScale A octave) qn volume 
    | index == 7 = triad (i) (diminishedScale A octave) qn volume
    | otherwise =  error "Invalid chord progression"
    where i = mapProgToKey2 k index octave
  
arpeggioInMajorProgression :: Int -> Key -> Octave -> [Pulse]
arpeggioInMajorProgression index k octave
    | index == 1 || index == 4 || index == 5 = arpeggio (i) (majorScale A octave) (replicate 4 (qn/4)) (replicate 4 (qn/4))
    | index == 2 || index == 3 || index == 6 = arpeggio (i) (minorScale A octave) (replicate 4 (qn/4))(replicate 4 (qn/4))
    | index == 7 = arpeggio (i) (diminishedScale A octave) (take 4 (repeat (qn/4))) (replicate 4 (qn/4))
    | otherwise =  error "Invalid chord progression"
    where i = mapProgToKey2 k index octave 

--higher order function
--createProgressionBar :: (Int -> Float -> [Pulse]) -> [Int] -> Semitone -> [Pulse]
createProgressionBar:: (t1 -> t2 -> t3 -> [a]) -> [t1] -> t2 -> t3 -> [a]
createProgressionBar f prog key octave = concat [concat $ replicate 4 $ f i key octave| i <- prog] 

--alternative melody creattion that plays in root key over all bars
randomMelody :: Scale Semitone -> Int -> p -> IO [Pulse]
randomMelody scale num_notes voices = do
    line <- pickNRandom (intervals scale) num_notes
    durs <- if num_notes <= 4 
            then pickNRandom [0.5, 1 .. 1] num_notes 
            else pickNRandom [0.25, 0.5 .. 1] num_notes
    vols <- pickNRandom [0.0, 0.1 .. 0.5] num_notes
    let notes = makeNote line durs vols
        playMelody = notes ++ notes ++ reverse notes ++ reverse notes
    return $ makeLine playMelody

--melody that follows chord progression
progressiveMelody :: Scale Semitone -> Octave -> Int -> p -> IO [Note]
progressiveMelody scale octave num_notes voices = do
    --TODO: account for voices
    
    line <- pickNRandom (intervals scale) num_notes
    vols <- pickNRandom [0.0, 0.05 .. 0.2] num_notes
    durs <- pickNSumCeiling [0.25, 0.5 .. 1] num_notes 
    let a = durs
        b = sum durs
        c = if b < 4 then a ++ [4-b] else a -- bar length has to be 4
        melody = makeNote line c vols 
    return melody

--really bad soulless walking bass 
walkingBass :: Scale Semitone -> Octave -> Int -> Voice -> IO [Note]
walkingBass scale octave num_notes voices = do
    l <- pickNRandom (intervals scale) 2
    --let line = (take 3 (intervals scale)) ++ [(intervals scale) !! 0]  --find out which one is more efficient
    let line = [(intervals scale !! 0)] ++ l ++ [(intervals scale !! 0)]
        melody = makeNote line (replicate 4 1) (replicate 4 0.1) 
    return melody

--Let a voice solo over a chord Progression
voiceProgression :: [Progression] -> Scale Semitone -> Key -> Octave -> NumNotes -> Voice -> IO [Pulse]
voiceProgression chordProg scale tk octave num_notes voices 
    | voices == Melody = do 
        seq <- sequence[progressiveMelody (transposeScale2 scale k tk) octave num_notes voices | k <- chords ]
        return  $ makeLine $ concat seq
    | voices == Bassline = do
        seq <- sequence[walkingBass (transposeScale2 scale k tk) octave num_notes voices | k <- chords ]
        return  $ makeLine $ concat seq
    | voices == ChordProgression = do
        return $ createProgressionBar chordInMinorProg chordProg tk octave

    where chords =  [mapProgToKey2 tk index octave | index <- chordProg]
    
    
---create zip N with n O num_voices?
--https://hoogle.haskell.org/?hoogle=%5BIO+a%5D+->+IO+%5Ba%5D   ---> idea for parallelization
createBars :: [Progression] -> Key  -> Octave -> MajMin -> Int -> NumNotes -> IO [Pulse]
createBars chordSeq k octave majMin num_bars num_notes = do
    
    let scale = if majMin == Major then chordScale k octave else  minorPentagonic k octave
    let bassScale = if majMin == Major then chordScale k (octave - 2) else minorScale k (octave - 2)
    let f = if majMin == Major then chordInMinorProg else chordInMajorProgression
    
    --melodyA <- randomMelody scale num_notes Melody 
    --let melAlt = concat $ replicate (length chordSeq * num_bars) melodyA
    
    let melody = voiceProgression chordSeq scale k octave num_notes Melody
    c1 <- concurrently melody melody
    c2 <- concurrently melody melody
    
    bass <- voiceProgression chordSeq bassScale k octave num_notes Bassline  
    chords <- voiceProgression chordSeq bassScale k octave num_notes ChordProgression
    let b = concat $ replicate (length chordSeq) $ bass
        chordpattern = concat $ replicate (length chordSeq) $ chords
        mel =  fst c1 ++ snd c1 ++ fst c2 ++ snd c2
        chords2 = if majMin == Major 
            then concat $ replicate (length chordSeq) $ createProgressionBar chordInMajorProgression chordSeq k octave
            else concat $ replicate (length chordSeq) $ createProgressionBar chordInMinorProg chordSeq k octave
        sound = zipWith3 (\x y z -> x + y + z) mel chords2 b
    --return [Bar key [Melody, ChordProgression] sound]
    return sound

createSheet :: [Progression] -> Key -> Octave -> MajMin-> Int -> NumNotes -> IO ()
createSheet chordSeq key octave majMin numBars num_notes
    | any (< 0) chordSeq || any (>7) chordSeq    = do error "Invalid chord progression"
    | otherwise = do
        bars <-  createBars chordSeq key octave majMin numBars num_notes
        let sheet = Sheet {_chordProg = chordSeq, _key = key, _majMin = majMin, _numBars = numBars, _barSeq = bars} --Reconrd Syntax
        saveAsWav (_barSeq sheet)

playIt :: Sheet -> IO ()
playIt = saveAsWav . _barSeq


--[1,2,3] `zip` [4,5,6] `zip` [4,5,6] THIS IS FREAKIN AWESOME USE IT!!!

---TODO
---PASS MINOR OR MAJOR TO CREATEBARS THEN DECIDE