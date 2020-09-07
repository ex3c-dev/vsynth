module Structures where

type DeviceSpecifier = Maybe String
type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitone = Float
type Beats = Float
type Octave = Float
type Volume = Float

--for now everything is in sharp because I said so.
--try and do own datatype that accepts alternatives... like either or???????
data Key = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs deriving (Enum, Show)

data Voice = Bassline | Melody | ChordProgression | Drums deriving (Enum, Show)

twelveBarBlues :: [Int]
twelveBarBlues = [1,1,1,1,4,4,1,1,5,4,1,5,5,4,1,5] 
axisOfAwesome :: [Int]
axisOfAwesome =  [1,5,6,4]

pessimistic :: [Int]
pessimistic = [6,4,1,5]

pop1 :: [Int]
pop1 = [1,4,5,1]

jazzCat :: [Int]
jazzCat = [2,5,1]

pachelbel :: [Int]
pachelbel = [1,5,6,3,4,1,4,5]

--WRITE 2 FUNCTIONS:
--ONE TO PLAY MUSIC IN PARALLEL 
-- ONE TO APPEND
--PLEASE DO THIS -.- 

key :: Enum a => a -> Semitone
key k = fromIntegral $ fromEnum k 

--psossibliy i should call this a triad lol
chord :: [Int]
chord = [0,2,4]

--add case for no octave given

majorScale :: [Semitone]
majorScale = [1, 3, 5, 6, 8, 10, 12, 13]

minorScale :: [Semitone]
minorScale = [1, 3, 4, 6, 8, 9, 11, 13]

diminishedScale :: [Semitone]
diminishedScale = [1,3,4,6,7,9,10,12,13] -- i .. think?

minorPentagonic :: [Semitone]
minorPentagonic = [1,4,6,8,11]

majorPentagonic :: [Semitone]
majorPentagonic = [1,3,5,8,10]

testtt:: [Semitone]
testtt = [1,5,8]

--minor pentagonic
--major pentagonic

data Note = Note {
    _semitone :: Semitone,
    _dur :: Beats,
    _vol :: Volume
} deriving (Show)

--eventually this could be updated on the fly maybe? instead of creating god knows how many bars. then use lenses to update
-- current key ? i dunno


--maybe... use this to show information in gui and update values with lenses or whatever?? meeeeh.
data Bar = Bar {
    _chordKey :: Key,    -- i think i guess
    _types :: [Voice],
    _music :: [Pulse]    --- meh?

} deriving (Show)

--add bpm?
data Sheet = Sheet {
    _numBars :: Int,
    _barSeq :: [Pulse], -- do i need this? i dont think so lmao
    _key :: Key,
    _chordProg :: [Int]
} deriving (Show)

---make function so that whatever durations i give they always sum up to 4

--make zipwithN function to combine N voices?

---then supply a list of bars and do it like runBand in example. parse the whole damn thing and just return a pulse
-- and thats that and fuck it. for every bar combine all the voices in it, and then append all the voices together
--iii guess i need to decide which informatin to put in there lel

--for now skip stuff like intro
--chordProgression position key 