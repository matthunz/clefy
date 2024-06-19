module Main where

import Data.Word (Word8)
import Options.Applicative
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
    Right xs -> putStrLn $ showFrets xs
runFrets _ = return ()

parseFrets :: String -> Either String [Word8]
parseFrets s = mapM readEither (words s)

showFrets :: [Word8] -> String
showFrets frets = unwords (map show frets)