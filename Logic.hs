module Logic where
import Data.Text (Text, pack)
import Data.List
import System.IO.Unsafe

datFile :: FilePath
datFile = "sdntDat.txt"
clubFile :: FilePath
clubFile = "ClubData.txt"

unJust (Just x) = x

data Student = Student
    { name    :: Text
    , grade   :: Int
    , choices :: [Text]
    }
  deriving (Show, Read, Eq)

clubl :: [(Text, (Int,Int))]
clubl = fan $ lines $ unsafePerformIO $ readFile $ clubFile
    where fan (x:y:z:zs) = (pack x,(read y, read z)) : fan zs
          fan _ = []

seniorSort :: [Student] -> [Student]
seniorSort = sortBy sortByGrade . nub
    where sortByGrade sdnt1 sdnt2
              |grade sdnt1 < grade sdnt2 = GT
              |grade sdnt1 > grade sdnt2 = LT
              |otherwise                 = EQ

updateAL al key val = case lookup key al of
    Nothing -> al
    Just v -> fst spl ++ [(key,val)] ++ drop 1 (snd spl)
        where spl = span (/=(key,v)) al

placeStudents :: [Student] -> ([(Text,[Student])], [Student]) -> ([(Text,[Student])], [Student])
placeStudents [] result = result
placeStudents (x:xs) result@(clubMbrs, unRes)
    |length chx < 1 = placeStudents xs (clubMbrs, unRes ++ [x])
    |length clubMLst < snd (unJust $ lookup (head chx) clubl) =
        placeStudents xs (updateAL clubMbrs (head chx) (clubMLst ++ [x]), unRes)
    |otherwise =
        placeStudents (Student (name x) (grade x) (drop 1 chx):xs) result
        where chx = choices x
              clubMLst = unJust $ lookup (head chx) clubMbrs

sortAll sdntLst = postSort $ placeStudents (seniorSort sdnts) (zip (map fst clubl) (repeat []),[])
    where sdnts = map read $ lines sdntLst

postSort :: ([(Text,[Student])], [Student]) -> ([(Text,[Student])], [Student])
postSort dat
    |length smallClubs > 0 =
        postSort $ placeStudents (map dropChoice $ concatMap snd smallClubs) (fltrOutLst smallClubs (fst dat), snd dat)
    |otherwise = dat
        where clubDat = fst dat
              clubMin c = fst (unJust $ lookup c clubl)
              smallClubs = filter (\(club,mbrs) -> length mbrs < clubMin club) clubDat
              fltrOutLst (x:xs) res = fltrOutLst xs (filter (/=x) res)
              fltrOutLst [] res = res
              dropChoice x = (Student (name x) (grade x) (drop 1 $ choices x))

