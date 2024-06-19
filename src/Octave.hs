-- |
-- Module      : Octave
-- Copyright   : (c) 2024 the Clefy authors
-- License     : Apache-2.0

module Octave where
import Data.Int (Int8)

data Octave
  = OctaveNeg1
  | OctaveZero
  | Octave1
  | Octave2
  | Octave3
  | Octave4
  | Octave5
  | Octave6
  | Octave7
  | Octave8

instance Show Octave where
  show o = show $ toInt o

toInt :: Octave -> Int8
toInt octave = case octave of
  OctaveNeg1 -> -1
  OctaveZero -> 0
  Octave1 -> 1
  Octave2 -> 2
  Octave3 -> 3
  Octave4 -> 4
  Octave5 -> 5
  Octave6 -> 6
  Octave7 -> 7
  Octave8 -> 8