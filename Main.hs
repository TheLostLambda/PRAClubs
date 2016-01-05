module Main where
import Yesod
import Logic
import Utils
import App

import Page.Theme
import Page.Form
--Refactor
--Also, run Hlint again

mkYesodDispatch "App" [parseRoutes|
/praClubs HomeR GET
/praClubs/submitted StudentR POST
/praClubs/results ResultR GET
|]

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost studentForm
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
    ((result, widget), enctype) <- runFormPost studentForm
    case result of
        FormSuccess student -> do
            liftIO $ appendFile datFile (show (sdntChoicesToLst student) ++ "\n")
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
getResultR = defaultLayout $ do
    sdntData <- liftIO $ readFile datFile
    let res = sortAll sdntData
        clubsl = map fst (fst res)
        unresolved = snd res
        members club = (\(Just x) -> x) $ lookup club (fst res)
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
main = warp 80 App
