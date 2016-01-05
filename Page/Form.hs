module Page.Form where
import Yesod
import Utils
import App


studentForm :: Html -> MForm Handler (FormResult FStudent, Widget)
studentForm = renderDivs $ FStudent
    <$> areq textField "Name: " Nothing
    <*> areq (selectFieldList grades) "Grade: " Nothing
    <*> areq (selectFieldList clubs) "First Choice Club: " Nothing
    <*> areq (selectFieldList clubs) "Second Choice Club: " Nothing
    <*> areq (selectFieldList clubs) "Third Choice Club: " Nothing
