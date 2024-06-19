-- |
-- Module      : Midi
-- Copyright   : (c) 2024 the Clefy authors
-- License     : Apache-2.0
module Midi where

import Data.Functor (($>))
import Data.Int (Int8)
import Data.Word (Word8)
import Octave
import Pitch
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (fromMaybe)

newtype MidiNote = MidiNote Word8
  deriving (Show)

midi :: Pitch -> Octave -> MidiNote
midi p o =
  MidiNote . fromIntegral $
    (Octave.toInt o + 1)
      * fromIntegral (Pitch.toByte Pitch.B + 1)
      + fromIntegral (Pitch.toByte p)

pitch :: MidiNote -> Pitch
pitch (MidiNote m) =
  let pitchClass = fromIntegral (m `mod` 12) :: Int
   in case pitchClass of
        0 -> C
        1 -> CSharp
        2 -> D
        3 -> DSharp
        4 -> E
        5 -> F
        6 -> FSharp
        7 -> G
        8 -> GSharp
        9 -> A
        10 -> ASharp
        11 -> B
        _ -> error "Unreachable"

octave :: MidiNote -> Octave
octave (MidiNote m) =
  let i = (fromIntegral (m `div` fromIntegral (Pitch.toByte Pitch.B + 1)) - 1) :: Int8
   in case i of
        -1 -> OctaveNeg1
        0 -> OctaveZero
        1 -> Octave1
        2 -> Octave2
        3 -> Octave3
        4 -> Octave4
        5 -> Octave5
        6 -> Octave6
        7 -> Octave7
        8 -> Octave8
        _ -> error "Unreachable"

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

parse :: Parser MidiNote
parse = do
  p <- Pitch.parse
  o <- optionMaybe $ choice
      [ string "-1" $> OctaveNeg1,
        char '0' $> OctaveZero,
        char '1' $> Octave1,
        char '2' $> Octave2,
        char '3' $> Octave3,
        char '4' $> Octave4,
        char '5' $> Octave5,
        char '6' $> Octave6,
        char '7' $> Octave7,
        char '8' $> Octave8
      ]

  return (midi p (fromMaybe Octave4 o))
