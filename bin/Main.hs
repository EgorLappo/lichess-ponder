module Main (main) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import Parse
import Ponder

data Options = Options
  { positionList :: !FilePath,
    output :: !FilePath,
    tmin :: !Int,
    tmax :: !Int
  }

optionsParser :: ParserInfo Options
optionsParser =
  info
    (helper <*> options)
    ( fullDesc
        <> progDesc "extract mean thinking times for positions from lichess games"
        <> header "lichess-ponder."
    )
  where
    options =
      Options
        <$> strOption
          ( long "positions"
              <> short 'p'
              <> help "list of positions, one per line"
              <> value "positions.txt"
              <> showDefault
          )
        <*> strOption
          ( long "output"
              <> short 'o'
              <> help "output file name"
              <> value "output.csv"
              <> showDefault
          )
        <*> option
          auto
          ( long "tmin"
              <> short 't'
              <> help "filter games to have timecontrol above tmin"
              <> value 300
              <> showDefault
          )
        <*> option
          auto
          ( long "tmax"
              <> short 'T'
              <> help "filter games to have timecontrol below tmin"
              <> value 300
              <> showDefault
          )

main :: IO ()
main = do
  opts <- execParser optionsParser
  -- the caller guarantees that nothing except algebraic notation
  -- appears in the position list
  positions <- T.lines . T.decodeASCII <$> BS.readFile (positionList opts)
  -- initialize the map
  let !moveMap = makeMoveMap positions
  games <- run (tmin opts, tmax opts)
  writeCSV (output opts) (formatMap $ process moveMap games)

writeCSV :: FilePath -> [(Text, Int, Double, Int)] -> IO ()
writeCSV fp = TIO.writeFile fp . T.unlines . map showRow
  where
    showRow (san, sk, cur, cnt) =
      T.intercalate "," [san, T.pack (show sk), T.pack (show cur), T.pack (show cnt)]
