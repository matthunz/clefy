-- |
-- Module      : Main
-- Copyright   : (c) 2024 the Clefy authors
-- License     : Apache-2.0
module Main where

import Data.Int (Int8)
import Data.Word (Word8)
import Midi (MidiNote (..), fretMidi, octave, pitch, standardTuning)
import qualified Midi
import Options.Applicative
import Text.Parsec (char, parse, sepBy)
import qualified Text.Parsec.Error
import qualified Text.Parsec.String
import Text.Read (readEither)

data Command = Default | Frets

data Args = Args
  {fretsFlagA :: Bool, transposeA :: Int8}

argsParser :: Parser Args
argsParser =
  Args
    <$> switch
      ( long "frets"
          <> short 'f'
          <> help "Input frets"
      )
    <*> option
      auto
      ( short 't'
          <> long "transpose"
          <> help "Transpose output"
          <> showDefault
          <> value 0
          <> metavar "HALFSTEPS"
      )

main :: IO ()
main = runFrets =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Music theory CLI")

runFrets :: Args -> IO ()
runFrets args = do
  frets <- getLine
  let notes =
        if fretsFlagA args
          then case parseFrets frets of
            Left e -> error (show e)
            Right xs -> zipWith fretMidi xs standardTuning
          else case parseNotes frets of
            Left e -> error (show e)
            Right xs -> xs
      output = map (\(MidiNote n) -> MidiNote (fromIntegral (fromIntegral n + transposeA args))) notes
   in putStrLn $
        unwords $
          map
            (\note -> show (pitch note) ++ show (octave note))
            output

notesParser :: Text.Parsec.String.Parser [MidiNote]
notesParser = sepBy Midi.parse (char ' ')

parseNotes :: String -> Either Text.Parsec.Error.ParseError [MidiNote]
parseNotes = parse notesParser ""

parseFrets :: String -> Either String [Word8]
parseFrets s = mapM readEither (words s)
