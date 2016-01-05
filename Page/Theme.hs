module Page.Theme where
import Yesod
import Utils
import App

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
