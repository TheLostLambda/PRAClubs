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
        <div .results>
            <h1>Student Club Placement
            <p>This page shows all current student submissions and the clubs into which they have been sorted. NOTE: These results are subject to change, and are NOT final until all student submissions have been recieved.
            <p>Hint: To find yourself on this page, press Ctrl-f and search your name.
            $forall club <- clubsl
                $if null (members club)
                $else
                    <h2> #{club}:
                    $forall (n, g) <- zip (map name $ members club) (map grade $ members club)
                        <p> #{n}, #{g}th
            $if null unresolved
            $else
                <h2> Unresolved:
                $forall (un, ug) <- zip (map name unresolved) (map grade unresolved)
                    <p> #{un}, #{ug}th
|]
