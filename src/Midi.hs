module Midi where

import Data.Word (Word8)
import Octave
import Pitch

newtype MidiNote = MidiNote Word8
  deriving (Show)

midi :: Pitch -> Octave -> MidiNote
midi p octave =
  MidiNote . fromIntegral $
    (Octave.toInt octave + 1)
      * fromIntegral (Pitch.toByte Pitch.B + 1)
      + fromIntegral (Pitch.toByte p)

pitch :: MidiNote -> Maybe Pitch
pitch (MidiNote m) =
  let pitchClass = fromIntegral (m `mod` 12) :: Int
   in case pitchClass of
        0 -> Just C
        1 -> Just CSharp
        2 -> Just D
        3 -> Just DSharp
        4 -> Just E
        5 -> Just F
        6 -> Just FSharp
        7 -> Just G
        8 -> Just GSharp
        9 -> Just A
        10 -> Just ASharp
        11 -> Just B
        _ -> Nothing

standardTuning :: [MidiNote]
standardTuning =
  [ midi Pitch.E Octave2,
    midi Pitch.A Octave2,
    midi Pitch.D Octave3,
    midi Pitch.G Octave3,
    midi Pitch.B Octave3,
    midi Pitch.E Octave4
  ]

fretMidi :: Word8 -> MidiNote -> MidiNote
fretMidi fret (MidiNote m) = MidiNote (fret + m)
