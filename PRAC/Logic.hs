module PRAC.Logic where
import PRAC.Utils

--Consider looking over this file for algorithmic fairness (and possible simplifications).

--The input list is reversed so that nub keeps to latest data on a student, and discards the old data instead.
--Also, in PRAC.Utils, there is a custom instance of Eq for Student. It ignores choices when checking, allowing
--students to overwrite their previous choices. The list is unreversed after the nub to preserve the
--fairness of the first come, first serve system.
seniorSort :: [Student] -> [Student]
seniorSort = sortBy sortByGrade . reverse . nub . reverse
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
placeStudents clubMap (x:xs) result@(clubMbrs, unRes)
    |length chx < 1 = placeStudents clubMap xs (clubMbrs, unRes ++ [x])
    |length clubMLst < snd (fromJust $ lookup (head chx) clubMap) =
        placeStudents clubMap xs (updateAL clubMbrs (head chx) (clubMLst ++ [x]), unRes)
    |otherwise =
        placeStudents clubMap (Student (name x) (grade x) (drop 1 chx):xs) result
        where chx = choices x
              clubMLst = fromJust $ lookup (head chx) clubMbrs

sortAll sdntLst clubMap = postSort clubMap $ placeStudents clubMap (seniorSort sdntLst) (zip (map fst clubMap) (repeat []),[])

postSort :: ClubMap -> ([(Text,[Student])], [Student]) -> ([(Text,[Student])], [Student])
postSort clubMap dat
    |length effectedSdnts > 0 =
        sortAll (seniorSort $ map dropChoice effectedSdnts ++ fltrOutLst effectedSdnts sdnts) clubMap
    |otherwise = dat
        where clubDat = fst dat
              clubMin c = fst (fromJust $ lookup c clubMap)
              smallClubs = filter (\(club,mbrs) -> length mbrs < clubMin club) clubDat
              sdnts = concatMap snd clubDat
              effectedSdnts = concatMap snd smallClubs
              fltrOutLst (x:xs) res = fltrOutLst xs (filter (/=x) res)
              fltrOutLst [] res = res
              dropChoice (Student n g (x:xs)) = Student n g xs
