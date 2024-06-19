module Main where

import Data.Word (Word8)
import Octave (Octave (..))
import qualified Octave
import Options.Applicative
import Pitch (Pitch)
import qualified Pitch
import Midi
import Text.Read (readEither)

data Command = Default | Frets

newtype Args = Args
  {commandA :: Command}

args :: Parser Args
args =
  Args
    <$> ( subparser
            ( command "frets" (info (pure Frets) (progDesc "Frets"))
            )
            <|> pure Default
        )

main :: IO ()
main = runFrets =<< execParser opts
  where
    opts = info (args <**> helper) (fullDesc <> progDesc "Music theory CLI")

runFrets :: Args -> IO ()
runFrets (Args Frets) = do
  frets <- getLine
  case parseFrets frets of
    Left e -> print e
    Right xs -> print $ map (\(x, y) -> pitch $ fretMidi x y) (zip xs standardTuning)
runFrets _ = return ()

parseFrets :: String -> Either String [Word8]
parseFrets s = mapM readEither (words s)

