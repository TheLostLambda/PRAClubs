module PRAC.Page.Form where
import PRAC.Utils
import PRAC.App
import Yesod

studentForm :: [(Text, (Int,Int))] -> Html -> MForm Handler (FormResult FStudent, Widget)
studentForm cMap =
    renderDivs $ FStudent
    <$> areq textField "Name: " Nothing
    <*> areq (selectFieldList grades) "Grade: " Nothing
    <*> areq (selectFieldList $ clubsToPairs cMap) "First Choice Club: " Nothing
    <*> areq (selectFieldList $ clubsToPairs cMap) "Second Choice Club: " Nothing
    <*> areq (selectFieldList $ clubsToPairs cMap) "Third Choice Club: " Nothing

submitSuccess :: WidgetT App IO ()
submitSuccess = do
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Club Signup
          <h3> Submitted
          <p> Your submission has been recieved, you're done!
|]

formWidget :: (ToWidget App w,ToMarkup e) => (w, e) -> WidgetT App IO ()
formWidget (widget, enctype) = do
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Club Signup
          <form method=post action=@{StudentR} enctype=#{enctype}>
              ^{widget}
              <button>Submit
    |]
