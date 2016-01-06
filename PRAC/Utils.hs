--Come up with a concrete purpose for this module, and rename it something more applicable.
module PRAC.Utils
    ( module Data.Maybe
    , module Data.Text
    , module Data.List
    , module Data.Yaml
    , datFile
    , clubFile
    , clubsToMap
    , clubsToPairs
    , grades
    , FStudent(..)
    , Student(..)
    , Club(..)
    , ClubMap
    , sdntChoicesToLst
    ) where

import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.List
import Data.Yaml

data Club = Club Text Int Int

instance FromJSON Club where
    parseJSON (Object v) = Club <$> v .: "name" <*> v .: "minSize" <*> v .: "maxSize"
    parseJSON invalid = fail $ "Failed to parse: " ++ show invalid

--Move these globals to a YAML config file. Write a loader for the config file here.
datFile :: FilePath
datFile = "sdntDat.txt"

clubFile :: FilePath
clubFile = "ClubData.txt"

type ClubMap = [(Text, (Int,Int))]

clubsToMap :: [Club] -> ClubMap
clubsToMap = map (\(Club n mn mx) -> (n,(mn,mx)))

clubsToPairs :: ClubMap -> [(Text, Text)]
clubsToPairs clubLst = [(x,x) | x <- map fst clubLst]

grades :: [(Text, Int)]
grades = zip (map (pack . (++"th") . show) [9..12]) [9..12]

data FStudent = FStudent
    { sN  :: Text, sG  :: Int, sC1 :: Text, sC2 :: Text, sC3 :: Text}

data Student = Student
    { name    :: Text
    , grade   :: Int
    , choices :: [Text]
    }
  deriving (Show, Read, Eq)

sdntChoicesToLst :: FStudent -> Student
sdntChoicesToLst s = Student
    (sN s)
    (sG s)
    [sC1 s, sC2 s, sC3 s]
