module Main where
import Data.Text (Text, pack)
import Yesod
import Logic

--Refactor
--Also, run Hlint again

data App = App

clubs :: [(Text, Text)]
clubs = [(x,x) | x <- map fst clubl]

grades :: [(Text, Int)]
grades = zip (map (pack . (++"th") . show) [9..12]) [9..12]

mkYesod "App" [parseRoutes|
/praClubs HomeR GET
/praClubs/submitted StudentR POST
/praClubs/results ResultR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data FStudent = FStudent
    { sN  :: Text, sG  :: Int, sC1 :: Text, sC2 :: Text, sC3 :: Text}

sdntChoicesToLst :: FStudent -> Student
sdntChoicesToLst s = Student
    (sN s)
    (sG s)
    [sC1 s, sC2 s, sC3 s]

studentForm :: Html -> MForm Handler (FormResult FStudent, Widget)
studentForm = renderDivs $ FStudent
    <$> areq textField "Name: " Nothing
    <*> areq (selectFieldList grades) "Grade: " Nothing
    <*> areq (selectFieldList clubs) "First Choice Club: " Nothing
    <*> areq (selectFieldList clubs) "Second Choice Club: " Nothing
    <*> areq (selectFieldList clubs) "Third Choice Club: " Nothing

pageTheme :: WidgetT App IO ()
pageTheme = do
        setTitle "PRA Club Chooser"
        toWidgetHead
            [hamlet|<link rel="icon" type="image/x-icon" href="http://www.prospectridgeacademy.org/favicon.ico"/>|]
        toWidgetHead
            [lucius|
                .formbox {
                    margin: auto;
                    width: 60%;
                    text-align: center;
                    font-family: Verdana, Geneva, sans-serif;
                    border: 5px ridge gold;
                    border-radius: 25px;
                    line-height: 200%;
                    padding: 10px;
                }
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
