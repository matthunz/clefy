module Main where

import Data.Word (Word8)
import Midi
import Octave (Octave (..))
import qualified Octave
import Options.Applicative
import Pitch (Pitch)
import qualified Pitch
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
    Right xs ->
      putStrLn $
        unwords $
        map
          (\(x, y) -> 
            let midi = fretMidi x y
            in show (pitch midi) ++ show (octave midi))
          (zip xs standardTuning)
runFrets _ = return ()

parseFrets :: String -> Either String [Word8]
parseFrets s = mapM readEither (words s)
