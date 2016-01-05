--Come up with a concrete purpose for this module, and rename it something more applicable.
module Utils
    ( module Data.Text
    , module Data.List
    , datFile
    , clubFile
    , unJust
    , clubl
    , clubs
    , grades
    , FStudent(..)
    , Student(..)
    , sdntChoicesToLst
    ) where

import Data.Text (Text, pack)
import Data.List
import System.IO.Unsafe

--Move these globals to a YAML config file. Write a loader for the config file here.
datFile :: FilePath
datFile = "sdntDat.txt"

clubFile :: FilePath
clubFile = "ClubData.txt"

unJust (Just x) = x

--Change so this reads YAML files
clubl :: [(Text, (Int,Int))]
clubl = fan $ lines $ unsafePerformIO $ readFile $ clubFile
    where fan (x:y:z:zs) = (pack x,(read y, read z)) : fan zs
          fan _ = []

clubs :: [(Text, Text)]
clubs = [(x,x) | x <- map fst clubl]

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
