{-# language OverloadedStrings #-}

module ExtendEffs where

import Prelude hiding (filter)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import System.Directory (doesFileExist)

data Efficiencies = Efficiencies
    {
        lmbda   :: !Double,
        roi0    :: !Double,
        roi1    :: !Double,
        roi2    :: !Double
    }
    deriving (Show, Eq)

-- Define how to get a Efficiences from a record (CSV row) by impolementing the
-- FromRecord type class
instance FromRecord Efficiencies where
  parseRecord eff = 
    Efficiencies 
      <$> eff .! 0 
      <*> eff .! 1 
      <*> eff .! 2 
      <*> eff .! 3 

type ErrorMsg = String
-- Type synonym to handle CSV contents
type CsvData = V.Vector Efficiencies

-- Function to read the CSV
--parseCsv :: FilePath -> IO (Either ErrorMsg CsvData)
--parseCsv filePath = do
--  fileExists <- doesFileExist filePath
--  if fileExists
--    then decode

readBlind :: IO ()
readBlind = do
  efficiencies <- readFile "2-40_AA_efficiency.txt"
  let effs = drop 2 (lines efficiencies)
  decoded <- decode NoHeader effs
  --print effs
  print decoded
