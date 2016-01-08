module PRAC.Page.Theme where
import PRAC.Utils
import PRAC.App
import Yesod

pageTheme :: WidgetT App IO ()
pageTheme = do
        setTitle "PRA Club Chooser"
        toWidgetHead
            [hamlet|<link rel="icon" type="image/x-icon" href="http://www.prospectridgeacademy.org/favicon.ico"/>|]
        toWidgetHead
            [lucius|
                body {
                    background-image: url(@{ResourceR bgPattern_png});
                }
                .formbox {
                    margin: auto;
                    width: 60%;
                    text-align: center;
                    font-family: "Comic Sans MS", cursive, sans-serif;
                    background-color: rgba(85, 85, 85, 0.4);
                    border: 10px groove gold;
                    line-height: 200%;
                    padding: 10px;
                }
                .results {
                    margin: auto;
                    width: 60%;
                    text-align: center;
                    font-family: "Comic Sans MS", cursive, sans-serif;
                    background-color: rgba(85, 85, 85, 0.4);
                    border: 10px groove gold;
                    padding: 10px;
                }
            |]
