module PRAC.App where
import qualified Data.ByteString.Char8 as BS
import PRAC.Utils
import Yesod

staticFiles "Resources/"

--Add config for clubMap, studentData, admin pass, static directory etc.
--adminPass :: IORef BS.ByteString
data App = App { clubM :: IORef ClubMap, resource :: Static}

mkYesodData "App" [parseRoutes|
/praClubs HomeR GET
/praClubs/submitted StudentR POST
/praClubs/results ResultR GET
/praClubs/resources ResourceR Static resource
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
