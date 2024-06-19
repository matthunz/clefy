-- |
-- Module      : Main
-- Copyright   : (c) 2024 the Clefy authors
-- License     : Apache-2.0
module Main where

import Data.Char (toLower)
import Data.Int (Int8)
import Data.Word (Word8)
import Midi (MidiNote (..), fretMidi, octave, pitch, standardTuning)
import qualified Midi
import Options.Applicative
import Text.Parsec (char, parse, sepBy)
import qualified Text.Parsec.Error
import qualified Text.Parsec.String
import Text.Read (readEither)

data Format = Note | Fret | Midi deriving (Show, Eq)

data Args = Args
  { transposeA :: Int8,
    inputA :: Format,
    outputA :: Format
  }

formatParser :: Char -> String -> String -> Parser Format
formatParser c l h =
  option
    parseFormat
    ( short c
        <> long l
        <> metavar "FRET | NOTE | MIDI"
        <> help h
        <> showDefault
        <> value Note
    )
  where
    parseFormat = do
      s <- str
      case map toLower s of
        "fret" -> return Fret
        "note" -> return Note
        "midi" -> return Midi
        _ -> fail "Invalid input type. Use 'fret' or 'note'."

argsParser :: Parser Args
argsParser =
  Args
    <$> option
      auto
      ( short 't'
          <> long "transpose"
          <> help "Transpose output"
          <> showDefault
          <> value 0
          <> metavar "HALFSTEPS"
      )
    <*> formatParser 'i' "input" "Input format"
    <*> formatParser 'o' "output" "Output format"

main :: IO ()
main = runFrets =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Music theory CLI")

runFrets :: Args -> IO ()
runFrets args = do
  line <- getLine
  let notes = case inputA args of
        Fret -> case parseInts line of
          Left e -> error (show e)
          Right xs -> zipWith fretMidi xs standardTuning
        Note -> case parseNotes line of
          Left e -> error (show e)
          Right xs -> xs
        Midi -> case parseInts line of
          Left e -> error (show e)
          Right xs -> map MidiNote xs
      output = map (\(MidiNote n) -> MidiNote (fromIntegral (fromIntegral n + transposeA args))) notes
   in putStrLn $
        unwords $
          map
            ( \note -> case outputA args of
                Note -> show (pitch note) ++ show (octave note)
                Fret -> error "TODO"
                Midi -> let (MidiNote n) = note in show n
            )
            output

notesParser :: Text.Parsec.String.Parser [MidiNote]
notesParser = sepBy Midi.parse (char ' ')

parseNotes :: String -> Either Text.Parsec.Error.ParseError [MidiNote]
parseNotes = parse notesParser ""

parseInts :: String -> Either String [Word8]
parseInts s = mapM readEither (words s)
