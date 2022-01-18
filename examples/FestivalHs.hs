{-# LANGUAGE TypeApplications, RecordWildCards #-}
module Main where

import qualified  Festival as Fest

import GHC.Natural
import Options.Applicative
import Control.Monad ((<=<))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

type SampleRate = Natural

data Target = TFile FilePath | StdOut
  deriving (Eq, Show, Read)

data Sink = Speaker | Wave SampleRate Target
  deriving (Eq, Show, Read)

data Source = SFile FilePath | StdIn
  deriving (Eq, Show, Read)

data Config
  = Config
  { source :: Source
  , sink :: Sink
  }

confP :: Parser Config
confP = Config
     <$> option (auto @Source)
         (  long "source"
         <> short 's'
         <> metavar "SOURCE"
         <> value StdIn
         <> showDefault
         <> help "Choose the source as either StdIn or 'SFile \"<path to file>\"'")
     <*> option (auto @Sink)
         (  long "target"
         <> short 'o'
         <> short 't'
         <> metavar "SINK"
         <> value Speaker
         <> showDefault
         <> help
           (  "Choose the sink (target) as either Speaker or 'Wave <samplerate> TARGET"
           <> " where TARGET is either StdOut or 'TFile \"<path>\"'"))

cli :: ParserInfo Config
cli = info (confP <**> helper)
      ( fullDesc
      <> progDesc
        (unlines [ "Example usage of haskell bindings to festival."
                 , "festival-hs is a simple applet for text to speech."
                 , "it can accept text from stdin or from a file, and the"
                 , "vocalizations can be passed to the system audio stack, or"
                 , "they can be written to file or stdout with a given samplerate."])
      <> header "festival-hs - simple example applet using haskell's bindings to festival.")

getText :: Source -> IO String
getText StdIn = getContents
getText (SFile x) = readFile x

writeWave :: Target -> ByteString -> IO ()
writeWave StdOut = BS.putStr
writeWave (TFile x) = BS.writeFile x

main :: IO ()
main = do
  Config{..} <- execParser cli
  text <- getText source
  Fest.initialize (Fest.InitConf True 6000000)
  case sink of
    Speaker -> Fest.sayText text
    Wave rate target -> do
      (Right wave) <- Fest.textToWave (fromIntegral rate) text
      writeWave target =<< Fest.sample wave
