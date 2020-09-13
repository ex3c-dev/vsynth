{-# LANGUAGE TemplateHaskell #-}
module Structures where

import Control.Lens

type DeviceSpecifier = Maybe String
type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitone = Float
type Beats = Float
type Octave = Float
type Volume = Float
type Progression = Int
type NumNotes = Int
type Interval = Float

data Key = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs deriving (Enum, Show, Eq, Bounded)

data Voice = Bassline | Melody | ChordProgression | Drums deriving (Enum, Show, Eq)

data MajMin = Major | Minor deriving (Show, Eq)

data Note = Note {
    _semitone :: Semitone,
    _dur :: Beats,
    _vol :: Volume
} deriving (Show, Eq)
makeLenses ''Note

data Sheet = Sheet {
    _numBars :: Int,
    _barSeq :: [Pulse],
    _key :: Key,
    _majMin :: MajMin,
    _chordProg :: [Int]
} deriving (Show)

newtype Scale a = Scale {intervals :: [a] } deriving (Show,Ord,Eq)

instance Functor Scale where
    fmap f (Scale []) = Scale []
    fmap f (Scale (x:xs)) = (Scale ((f x):(fmap f xs)))

chord :: [Progression]
chord = [0,2,4]

majScale :: [Interval]
majScale = [1, 3, 5, 6, 8, 10, 12, 13]

minScale :: [Interval]
minScale = [1, 3, 4, 6, 8, 9, 11, 13]

dimScale :: [Interval]
dimScale = [1, 3, 4, 6, 7, 9, 10, 12, 13] 

augScale :: [Interval]
augScale = [1, 4, 5, 8, 9, 12, 13]

minPentagonic :: [Interval]
minPentagonic = [1,4,6,8,11]

fifthsmScale:: [Interval]
fifthsmScale = [1,4,8,13]

majPentagonic :: [Interval]
majPentagonic = [1,3,5,8,10]

fifthsMScale:: [Interval]
fifthsMScale = [1,3,8,13]

twelveBarBlues :: [Progression]
twelveBarBlues = [1,1,1,1,4,4,1,1,5,4,1,5,5,4,1,5] 

axisOfAwesome :: [Progression]
axisOfAwesome =  [1,5,6,4]

pessimistic :: [Progression]
pessimistic = [6,4,1,5]

pop1 :: [Progression]
pop1 = [1,4,5,1]

jazzCat :: [Progression]
jazzCat = [2,5,1]

pachelbel :: [Progression]
pachelbel = [1,5,6,3,4,1,4,5]

minorProg :: [Progression]
minorProg = [1,4,5,1]
