--Come up with a concrete purpose for this module, and rename it something more applicable.
module PRAC.Utils
    ( module Data.Maybe
    , module Data.Text
    , module Data.List
    , module Data.Yaml
    , clubsToMap
    , clubsToPairs
    , grades
    , FStudent(..)
    , Student(..)
    , Club(..)
    , ClubMap
    , fromStudent
    , toStudent
    ) where

import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.List
import Data.Yaml

data Club = Club Text Int Int deriving (Show, Read, Eq)

instance FromJSON Club where
    parseJSON (Object v) = Club <$> v .: "name" <*> v .: "minSize" <*> v .: "maxSize"
    parseJSON invalid = fail $ "Failed to parse: " ++ show invalid

data FStudent = FStudent
    { sN  :: Text, sG  :: Int, sC1 :: Text, sC2 :: Text, sC3 :: Text}

instance ToJSON FStudent where
    toJSON (FStudent n g f s t) = array [object ["name" .= n, "grade" .= g, "1st" .= f, "2nd" .= s, "3rd" .= t]]

instance FromJSON FStudent where
    parseJSON (Object v) = FStudent <$> v .: "name" <*> v .: "grade" <*> v .: "1st" <*> v .: "2nd" <*> v .: "3rd"
    parseJSON invalid = fail $ "Failed to parse: " ++ show invalid

data Student = Student
    { name    :: Text
    , grade   :: Int
    , choices :: [Text]
    }
  deriving (Show, Read)

instance Eq Student where
    (==) (Student n1 g1 _) (Student n2 g2 _) = n1 == n2 && g1 ==g2

fromStudent :: Student -> FStudent
fromStudent s = FStudent
    (name s)
    (grade s)
    (choices s !! 0)
    (choices s !! 1)
    (choices s !! 2)

toStudent :: FStudent -> Student
toStudent s = Student
    (sN s)
    (sG s)
    [sC1 s, sC2 s, sC3 s]

type ClubMap = [(Text, (Int,Int))]

clubsToMap :: [Club] -> ClubMap
clubsToMap = map (\(Club n mn mx) -> (n,(mn,mx)))

clubsToPairs :: ClubMap -> [(Text, Text)]
clubsToPairs clubLst = [(x,x) | x <- map fst clubLst]

grades :: [(Text, Int)]
grades = zip (map (pack . (++"th") . show) [9..12]) [9..12]
