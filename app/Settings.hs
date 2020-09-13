module Settings where 
import Structures

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