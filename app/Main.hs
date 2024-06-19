-- |
-- Module      : Main
-- Copyright   : (c) 2024 the Clefy authors
-- License     : Apache-2.0
module Main where

import Data.Word (Word8)
import Midi (MidiNote, fretMidi, octave, pitch, standardTuning)
import qualified Midi
import Octave (Octave (..))
import qualified Octave
import Options.Applicative
import Pitch (Pitch)
import qualified Pitch
import Text.Parsec (char, parse, sepBy)
import qualified Text.Parsec.Error
import qualified Text.Parsec.String
import Text.Read (readEither)

data Command = Default | Frets

newtype Args = Args
  {fretsA :: Bool}

args :: Parser Args
args =
  Args
    <$> ( switch
            ( long "frets"
                <> short 'f'
                <> help "Input frets"
            )
        )

main :: IO ()
main = runFrets =<< execParser opts
  where
    opts = info (args <**> helper) (fullDesc <> progDesc "Music theory CLI")

runFrets :: Args -> IO ()
runFrets args = do
  frets <- getLine
  let notes =
        if fretsA args
          then case parseFrets frets of
            Left e -> error (show e)
            Right xs -> map (\(x, y) -> fretMidi x y) (zip xs standardTuning)
          else case parseNotes frets of
            Left e -> error (show e)
            Right xs -> xs
   in putStrLn $
        unwords $
          map
            (\note -> show (pitch note) ++ show (octave note))
            notes

notesParser :: Text.Parsec.String.Parser [MidiNote]
notesParser = sepBy Midi.parse (char ' ')

parseNotes :: String -> Either Text.Parsec.Error.ParseError [MidiNote]
parseNotes input = parse notesParser "" input

parseFrets :: String -> Either String [Word8]
parseFrets s = mapM readEither (words s)
