module PRAC.Page.Results where
import PRAC.Utils
import PRAC.Logic
import PRAC.App
import Yesod

resultsPage :: [FStudent] -> ClubMap -> WidgetT App IO ()
resultsPage sdntDat cMap = do
        let res = sortAll (map toStudent sdntDat) cMap
            clubsl = map fst (fst res)
            unresolved = snd res
            members club = fromJust $ lookup club (fst res)
        [whamlet|
        <div .formbox>
            $forall club <- clubsl
                $if null (members club)
                $else
                    <h2> #{club}:
                    $forall (n, g) <- zip (map name $ members club) (map grade $ members club)
                        <p> #{n}, #{g}th
            $if null unresolved
            $else
                <h2> Unresolved:
                <ul>
                    $forall (un, ug) <- zip (map name unresolved) (map grade unresolved)
                        <li> #{un}, #{ug}th
|]
