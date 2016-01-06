module Main where
import PRAC.Logic
import PRAC.Utils
import PRAC.App
import Yesod

import PRAC.Page.Theme
import PRAC.Page.Form
--Refactor
--Also, run Hlint again

mkYesodDispatch "App" [parseRoutes|
/praClubs HomeR GET
/praClubs/submitted StudentR POST
/praClubs/results ResultR GET
|]

getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    (widget, enctype) <- generateFormPost (studentForm clubM)
    defaultLayout $ do
        pageTheme
        [whamlet|
            <div .formbox>
                <h1> Prospect Ridge Academy Club Signup
                <form method=post action=@{StudentR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
        |]

postStudentR :: Handler Html
postStudentR = do
    App {..} <- getYesod
    ((result, widget), enctype) <- runFormPost (studentForm clubM)
    case result of
        FormSuccess student -> do
            liftIO $ appendFile datFile (show (sdntChoicesToLst student) ++ "\n") --Bad! Bad Text file! Why can't you be more like your brother Yaml?
            defaultLayout $ do
                pageTheme
                [whamlet|
                    <div .formbox>
                        <h1> Prospect Ridge Academy Club Signup
                        <h3> Submitted
                        <p> Your submission has been recieved, you're done!
                |]
        _ -> defaultLayout [whamlet|
                <h1> Nice going, now look at what you have done. You messed up bad...
            |]

getResultR :: Handler Html
getResultR = do
    App {..} <- getYesod
    sdntData <- liftIO $ readFile datFile
    let res = sortAll sdntData clubM
        clubsl = map fst (fst res)
        unresolved = snd res
        members club = (\(Just x) -> x) $ lookup club (fst res)
    defaultLayout $ do
        pageTheme
        [whamlet|
            $forall club <- clubsl
                <h3> #{club}:
                <ul>
                    $forall (n, g) <- zip (map name $ members club) (map grade $ members club)
                        <li> #{n}, #{g}th
            $if null unresolved
            $else
                <h3> Unresolved:
                <ul>
                    $forall (un, ug) <- zip (map name unresolved) (map grade unresolved)
                        <li> #{un}, #{ug}th
        |]


main :: IO ()
main = do
    clubLst <- decodeFile "clubData.yaml" :: IO (Maybe [Club])
    warp 80 App {clubM = clubsToMap (fromJust clubLst)}
