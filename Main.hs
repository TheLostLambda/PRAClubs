module Main where
import qualified Data.ByteString.Char8 as BS
import PRAC.Logic
import PRAC.Utils
import PRAC.App
import Yesod

import PRAC.Page.Results
import PRAC.Page.Theme
import PRAC.Page.Form

mkYesodDispatch "App" [parseRoutes|
/praClubs HomeR GET
/praClubs/submitted StudentR POST
/praClubs/results ResultR GET
/praClubs/resources ResourceR Static resource
|]

getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    f <- generateFormPost (studentForm clubM)
    defaultLayout $ do
        pageTheme
        formWidget f

postStudentR :: Handler Html
postStudentR = do
    App {..} <- getYesod
    ((result, widget), enctype) <- runFormPost (studentForm clubM)
    case result of
        FormSuccess fStudent -> do
            liftIO $ BS.appendFile "sdntData.yaml" (encode [fStudent])
            defaultLayout $ do
                pageTheme
                submitSuccess

getResultR :: Handler Html
getResultR = do
    App {..} <- getYesod
    sdntData <- liftIO $ decodeFile "sdntData.yaml"
    defaultLayout $ do
        pageTheme
        resultsPage (fromJust sdntData) clubM

main :: IO ()
main = do
    res <- static "Resources/"
    clubLst <- decodeFile "clubData.yaml"
    warp 80 App {clubM = clubsToMap (fromJust clubLst), resource = res}
