module Main where
import Structures
import Utils
import IOShit
import Lib
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
import Data.IORef

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

---------------------------------------------------------------------------------------------------------------------

volume :: Volume
volume = 0.2

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

makeKey :: [Pulse] -> Semitone -> Octave -> [Pulse]
makeKey line key octave = [x + key + (octave * 12)| x <- line]

mapKey :: [Semitone] -> Semitone -> Octave -> [Pulse]
mapKey line key octave = [x + key + (octave * 12)| x <- line]

getIndice :: Semitone -> Int -> Octave -> Semitone
getIndice k indice octave =
    let scale = mapKey majorScale k octave
    in scale !! (indice -1)

--zip function takes care that everything stays in a bar :D
makeNote :: [Semitone] -> [Beats] -> [Volume] -> [Note]
makeNote line dur vol = [Note x y z |(x, y, z) <- zip3 line dur vol]

arpeggio :: Semitone -> [Semitone] -> [Beats] -> [Volume] -> Octave -> [Pulse]
arpeggio key tscale dur vol octave = 
    let scale = (makeKey tscale key octave) 
        arpeg = chord ++ [0]
        chordDone = [scale !! x | x <- arpeg]
        notes = makeNote chordDone dur vol
    in makeLine notes

---try and do this with list comprehension with i in length (chord)
triad :: Semitone -> [Semitone] -> Beats -> Volume -> Octave -> [Pulse]
triad key tscale dur vol octave = 
    let scale = (makeKey tscale key octave)
        v = take (length chord) $ repeat vol
        d = take (length chord) $ repeat dur
        chordDone = [scale !! x | x <- chord]
        notes = makeNote chordDone d v
    in concat [zipWith3 (\x y z -> x + y + z) (makeLine ([notes !! 0])) (makeLine ([notes !! 1])) (makeLine ([notes !! 2]))]

--scope bindings over several guarded equations
chordInProg :: Int -> Float -> [Pulse]
chordInProg index k | index == 1 || index == 4 || index == 5 = triad (i - 1) majorScale qn volume octave
                    | index == 2 || index == 3 || index == 6 = triad (i - 1) minorScale qn volume octave
                    | index == 7 = triad (i - 1) diminishedScale qn volume octave
                      where i = getIndice k index octave

arpeggioInProg :: Int -> Float -> [Pulse]
arpeggioInProg index k | index == 1 || index == 4 || index == 5 = arpeggio (i - 1) majorScale (take 4 (repeat qn)) (take 4 (repeat volume)) octave
                       | index == 2 || index == 3 || index == 6 = arpeggio (i - 1) minorScale (take 4 (repeat qn)) (take 4 (repeat volume)) octave
                       | index == 7 = arpeggio (i - 1) diminishedScale (take 4 (repeat qn)) (take 4 (repeat volume)) octave
                         where i = getIndice k index octave 

--higher order function
createProgressionBar :: (Int -> Float -> [Pulse]) -> [Int] -> Semitone -> [Pulse]
createProgressionBar f prog key = concat [concat $ replicate 4 (f i key) | i <- prog] --- as partes funcionam lol mas o todo nao


randomMel :: Semitone -> [Semitone] -> Octave -> Int -> Voice -> IO [Pulse]
randomMel key scale octave num_notes voices = do
    --TODO: account for voices
    let s = makeKey scale key octave
    line <- pickNFromScale s num_notes
    --TODO: finish this
    durs <- if num_notes <= 4 
            then pickNFromScale [0.5, 1 .. 2] num_notes        --durs <- pickNFromScale [0.25, 0.75 .. 4] num_notes
            else pickNFromScale [0.25, 0.5 .. 1] num_notes
    vols <- pickNFromScale [0.0, 0.1 .. 1] num_notes
    let notes = makeNote line durs vols
        playMelody = notes ++ notes ++ reverse notes 
        lines = makeLine playMelody
    return lines
 
--https://hoogle.haskell.org/?hoogle=%5BIO+a%5D+->+IO+%5Ba%5D
repeatMel :: [Int] -> [Semitone] -> Semitone -> Octave -> Voice -> IO [Pulse]
repeatMel chordProg scale k octave voices = do
    let chords =  [getIndice k index octave | index <- chordProg]
    seq <- sequence [randomMel (k - 1) scale octave 4 voices | k <- chords ]
    let c = concat seq
    return c

---eventually fuck bars lmao
---create zip N with n O num_voices?
---zip fuńction here takes care of the beats misa thinks
createBars :: [Int] -> Key  -> Octave -> Int -> Int -> IO [Pulse]
createBars chordSeq k octave num_bars num_notes = do
    
    melody <- randomMel (key(k)) majorPentagonic octave num_notes Melody   ----make it take korrekt duration as input
    let mel = concat $ replicate (length chordSeq * num_bars) melody
        chords = concat $ replicate num_bars $ createProgressionBar chordInProg chordSeq (key(k))   ----take chordprogression as input
    ---in [Bar key [Melody, ChordProgression] melody]
        sound = zipWith (+) mel chords

    --return [Bar key [Melody, ChordProgression] sound]
    return sound

createBars3 :: [Int] -> Key  -> Octave -> Int -> Int -> IO [Pulse]
createBars3 chordSeq k octave num_bars num_notes = do
    
    melody <- repeatMel chordSeq majorPentagonic (key(k)) octave Melody   ----make it take korrekt duration as input
    let chords = concat $ replicate num_bars $ createProgressionBar chordInProg chordSeq (key(k))
        mel = concat $ replicate (length chordSeq * num_bars) melody
        sound = zipWith (+) mel chords

    --return [Bar key [Melody, ChordProgression] sound]
    return sound

createBars2 :: [Int] -> Key  -> Octave -> Int -> Int -> IO [Pulse]
createBars2 chordSeq k octave num_bars num_notes= do
    
    melodyA <- repeatMel chordSeq majorPentagonic (key(k)) octave Melody  ----make it take korrekt duration as input
    melodyB <- randomMel (key(k)) majorPentagonic octave num_notes Melody
    --let m = zipWith (+) melodyA melodyB
    let m = melodyA ++ melodyB ++ melodyA
        mel = concat $ replicate (length chordSeq * num_bars) m
        chords = concat $ replicate num_bars $ createProgressionBar chordInProg chordSeq (key(k))   ----take chordprogression as input
    ---in [Bar key [Melody, ChordProgression] melody]
        sound = zipWith (+) mel chords
    --return [Bar key [Melody, ChordProgression] sound]
    return sound

-- add options for voices
-- as in MOAH
--would be nice to print the bar sequence but MEH.
createSheet :: [Int] -> Key -> Octave -> Int -> Int -> IO ()
createSheet chordSeq key octave numBars num_notes= do
    bars <-  createBars chordSeq key octave numBars num_notes
    let sheet = Sheet {_chordProg = chordSeq, _key = key, _numBars = numBars, _barSeq = bars} 
    saveAsWav (_barSeq sheet)


playIt :: Sheet -> IO ()
playIt = saveAsWav . _barSeq

main :: IO ()
main = do

   withProgNameAndArgs runALUT $ \progName args -> do

        unless (length args == 1) $ do
            hPutStrLn stderr ("usage: " ++ progName ++ " <fileName>")
            exitFailure
        playFile (head args)

        createSheet axisOfAwesome A 4 0 4
