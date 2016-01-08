module PRAC.App where
import PRAC.Utils
import Yesod

staticFiles "Resources/"

--Add config for clubMap, studentData, admin pass, static directory etc.
data App = App { clubM :: ClubMap, resource :: Static}

mkYesodData "App" [parseRoutes|
/praClubs HomeR GET
/praClubs/submitted StudentR POST
/praClubs/results ResultR GET
/praClubs/resources ResourceR Static resource
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
