module Music where 

import Structures
import Utils
import IOFunctions
import Lib
import Functions
import Settings
import Control.Monad.Random
import Control.Monad.Par
import Data.List 
import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent
import Control.DeepSeq
import Data.Maybe 


arpeggio:: Semitone -> Scale Semitone -> [Beats] -> [Volume] -> [Pulse]
arpeggio key tscale dur vol = 
    let scale = makeScale tscale key
        arpeg = chord ++ [0]
        chordDone = [(intervals scale) !! x | x <- arpeg]
        notes = makeNote chordDone dur vol
    in makeLine notes

triad :: Semitone -> Scale Semitone -> Beats -> Volume -> [Pulse]
triad key tscale dur vol = 
    let scale = makeScale tscale key
        v = take (length chord) $ repeat vol
        d = take (length chord) $ repeat dur
        chordDone = [(intervals scale) !! x | x <- chord]
        notes = makeNote chordDone d v
    in concat [zipWith3 (\x y z -> x + y + z) (makeLine ([notes !! 0])) (makeLine ([notes !! 1])) (makeLine ([notes !! 2]))]

--rules for minor progression
chordInMinorProg :: Int -> Key -> Octave -> [Pulse]
chordInMinorProg index k octave 
    | index == 5 || index == 6 = triad (i) (majorScale A octave) qn volume
    | index == 1 || index == 4 = triad (i) (minorScale A octave) qn volume 
    | index == 2 || index == 7 = triad (i) (diminishedScale A octave) qn volume
    | index == 3 = triad (i) (augmentedScale A octave) qn volume
    | otherwise =  error "Invalid chord progression"
    where 
     i :: Semitone 
     i = mapProgToKey2 k index octave

--rules for mahor progression
chordInMajorProgression :: Int -> Key -> Octave -> [Pulse]
chordInMajorProgression index k octave 
    | index == 1 || index == 4 || index == 5 = triad (i) (majorScale A octave) qn volume
    | index == 2 || index == 3 || index == 6 = triad (i) (minorScale A octave) qn volume 
    | index == 7 = triad (i) (diminishedScale A octave) qn volume
    | otherwise =  error "Invalid chord progression"
    where
     i :: Semitone
     i = mapProgToKey2 k index octave
  
arpeggioInMajorProgression :: Int -> Key -> Octave -> [Pulse]
arpeggioInMajorProgression index k octave
    | index == 1 || index == 4 || index == 5 = arpeggio (i) (majorScale A octave) (replicate 4 (qn/4)) (replicate 4 (qn/4))
    | index == 2 || index == 3 || index == 6 = arpeggio (i) (minorScale A octave) (replicate 4 (qn/4))(replicate 4 (qn/4))
    | index == 7 = arpeggio (i) (diminishedScale A octave) (take 4 (repeat (qn/4))) (replicate 4 (qn/4))
    | otherwise =  error "Invalid chord progression"
    where
     i :: Semitone
     i = mapProgToKey2 k index octave 

-- Function that creates a progression 
createProgressionBar :: (Int -> Key ->  Octave -> [Pulse]) -> [Progression] -> Key  -> Octave-> [Pulse]
createProgressionBar f prog key octave = concat [concat $ replicate 4 $ f i key octave| i <- prog] 

--alternative melody creation that plays in root key over all bars
randomMelody :: MonadRandom m => Scale Semitone -> Int -> p -> m [Pulse]
randomMelody scale num_notes voices = do
    line <- pickNRandom (intervals scale) num_notes
    durs <- if num_notes <= 4 
            then pickNRandom [0.5, 1 .. 1] num_notes 
            else pickNRandom [0.25, 0.5 .. 1] num_notes
    vols <- pickNRandom [0.0, 0.05 .. 0.5] num_notes
    let notes = makeNote line durs vols
    return $ makeLine $ notes ++ notes ++ reverse notes

--melody that follows chord progression
progressiveMelody :: MonadRandom m => Scale Semitone -> Octave -> Int -> Voice -> m [Note]
progressiveMelody scale octave num_notes voices = do    
    line <- pickNRandom (intervals scale) num_notes
    vols <- pickNRandom [0.0, 0.05 .. 0.2] num_notes
    durs <- pickNSumCeiling [0.25, 0.5 .. 1] num_notes 
    let c = if sum durs < 4 then durs ++ [4 - sum durs ] else durs 
        v = filterReplace ( < ceiling 0.1) vols  0
    return $ makeNote line c v

--soulless walking bass 
walkingBass :: MonadRandom m => Scale Semitone -> Octave -> Int -> Voice -> m [Note]
walkingBass scale octave num_notes voices = do
    l <- pickNRandom (intervals scale) 2
    let line = [(intervals scale !! 0)] ++ l ++ [(intervals scale !! 0)]
    return $ makeNote line (replicate 4 1) (replicate 4 0.1) 

--Let a voice follow over a chord Progression
voiceProgression :: MonadRandom m => [Int] -> Key -> Octave -> MajMin -> Int -> Voice -> m [Pulse]
voiceProgression chordProg tk octave majMin num_notes voices 
    | voices == Melody = do 
        let scale = if majMin == Major then chordScale tk octave else  minorPentagonic tk octave
        seq <- sequence[progressiveMelody (transposeScale2 scale k tk) octave num_notes voices | k <- chords ]
        return  $ makeLine $ concat seq
    | voices == Bassline = do
        let bassScale = if majMin == Major then chordScale tk (octave - 2) else minorScale tk (octave - 2)
        seq <- sequence[walkingBass (transposeScale2 bassScale k tk) octave num_notes voices | k <- chords ]
        return  $ makeLine $ concat seq
    | voices == ChordProgression = do
        let f = if majMin == Major then chordInMajorProgression else chordInMinorProg  
        return $ createProgressionBar f chordProg tk octave
    where
     chords :: [Semitone]
     chords =  [mapProgToKey2 tk index octave | index <- chordProg]

createBars :: [Progression] -> Key  -> Octave -> MajMin -> Int -> NumNotes -> IO [Pulse]
createBars chordSeq k octave majMin num_bars num_notes = do

    --melodyA <- randomMelody scale num_notes Melody 
    --let melAlt = concat $ replicate (length chordSeq * num_bars) melodyA

        --let melody = voiceProgression chordSeq  k octave majMin num_notes Melody
        --bass <- voiceProgression chordSeq k octave majMin num_notes Bassline  
        --chords <- voiceProgression chordSeq k octave majMin num_notes ChordProgression


    c1 <- concurrently (voiceProgression chordSeq  k octave majMin num_notes Melody) (voiceProgression chordSeq  k octave majMin num_notes Melody)
    c2 <- concurrently (voiceProgression chordSeq  k octave majMin num_notes Melody) (voiceProgression chordSeq  k octave majMin num_notes Melody)
    c3 <- concurrently (voiceProgression chordSeq k octave majMin num_notes Bassline ) (voiceProgression chordSeq k octave majMin num_notes ChordProgression)

    let b = concat $ replicate (num_bars) $ fst c3
        chordpattern = concat $ replicate (num_bars) $ snd c3
        mel = fst c1 ++ snd c1 ++ fst c2 ++ snd c2
        mels = concat $ replicate (num_bars) $ fst c1 ++ snd c1 ++ fst c2 ++ snd c2

    return $ zipWith3 (\x y z -> x + y + z) mels chordpattern b


createSheet:: FilePath -> [Progression] -> Key -> Octave -> MajMin -> Int -> NumNotes -> IO ()
createSheet filePath chordSeq key octave majMin numBars num_notes
    | any (< 0) chordSeq || any (>7) chordSeq    = do error "Invalid chord progression"
    | otherwise = do
        bars <-  createBars chordSeq key octave majMin numBars num_notes
        let sheet = Sheet {_chordProg = chordSeq, _key = key, _majMin = majMin, _numBars = numBars, _barSeq = bars} --Record Syntax
        saveAsWav (_barSeq sheet) filePath

