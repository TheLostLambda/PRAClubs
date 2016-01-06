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
