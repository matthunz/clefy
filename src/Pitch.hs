module Pitch where
import Data.Word (Word8)

data Pitch
  = C
  | CSharp
  | D
  | DSharp
  | E
  | F
  | FSharp
  | G
  | GSharp
  | A
  | ASharp
  | B
  deriving (Show)

toByte :: Pitch -> Word8
toByte pitch = case pitch of
  C -> 0
  CSharp -> 1
  D -> 2
  DSharp -> 3
  E -> 4
  F -> 5
  FSharp -> 6
  G -> 7
  GSharp -> 8
  A -> 9
  ASharp -> 10
  B -> 11