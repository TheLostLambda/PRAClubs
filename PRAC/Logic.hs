module PRAC.Logic where
import PRAC.Utils

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

placeStudents :: ClubMap -> [Student] -> ([(Text,[Student])], [Student]) -> ([(Text,[Student])], [Student])
placeStudents _ [] result = result
placeStudents clubl (x:xs) result@(clubMbrs, unRes)
    |length chx < 1 = placeStudents clubl xs (clubMbrs, unRes ++ [x])
    |length clubMLst < snd (fromJust $ lookup (head chx) clubl) =
        placeStudents clubl xs (updateAL clubMbrs (head chx) (clubMLst ++ [x]), unRes)
    |otherwise =
        placeStudents clubl (Student (name x) (grade x) (drop 1 chx):xs) result
        where chx = choices x
              clubMLst = fromJust $ lookup (head chx) clubMbrs

sortAll sdntLst clubMap = postSort clubMap $ placeStudents clubMap (seniorSort sdnts) (zip (map fst clubMap) (repeat []),[])
    where sdnts = map read $ lines sdntLst

postSort :: ClubMap -> ([(Text,[Student])], [Student]) -> ([(Text,[Student])], [Student])
postSort clubl dat
    |length smallClubs > 0 =
        postSort clubl $ placeStudents clubl (map dropChoice $ concatMap snd smallClubs) (fltrOutLst smallClubs (fst dat), snd dat)
    |otherwise = dat
        where clubDat = fst dat
              clubMin c = fst (fromJust $ lookup c clubl)
              smallClubs = filter (\(club,mbrs) -> length mbrs < clubMin club) clubDat
              fltrOutLst (x:xs) res = fltrOutLst xs (filter (/=x) res)
              fltrOutLst [] res = res
              dropChoice x = (Student (name x) (grade x) (drop 1 $ choices x))
