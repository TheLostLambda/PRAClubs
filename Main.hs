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
    clubMap <- liftIO $ readIORef clubM
    f <- generateFormPost (studentForm clubMap)
    defaultLayout $ do
        pageTheme
        formWidget f

postStudentR :: Handler Html
postStudentR = do
    App {..} <- getYesod
    clubMap <- liftIO $ readIORef clubM
    ((result, widget), enctype) <- runFormPost (studentForm clubMap)
    case result of
        FormSuccess fStudent -> do
            liftIO $ BS.appendFile "sdntData.yaml" (encode [fStudent])
            defaultLayout $ do
                pageTheme
                submitSuccess

getResultR :: Handler Html
getResultR = do
    App {..} <- getYesod
    clubMap <- liftIO $ readIORef clubM
    sdntData <- liftIO $ decodeFile "sdntData.yaml"
    defaultLayout $ do
        pageTheme
        resultsPage (fromJust sdntData) clubMap

main :: IO ()
main = do
    res <- static "Resources/"
    clubLst <- decodeFile "clubData.yaml"
    clubMap <- newIORef (clubsToMap $ fromJust clubLst)
    warp 80 App {clubM = clubMap, resource = res}
