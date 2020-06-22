module Roll20.Tabbed
    ( TabbedSheet(..)
    , TabbedSheetSection(..)
    , tabbed
    ) where

import Control.Monad ( forM_ )
import qualified Data.Text as T
import Language.Javascript.JMacro
import qualified Clay as C
import qualified Text.PrettyPrint.Leijen.Text as PP

import Roll20.Field
import Roll20.SheetWorker
import Roll20.Tabbed.Styles
import Roll20.Tabbed.Types

import Lucid

--

tabbed :: forall ts. TabbedSheet ts => (Html (), C.Css)
tabbed = (doc', css)
    where
        doc' = do
            doc
            script_ [type_ "text/worker"] $ toHtmlRaw jsSrc
            getRollTemplates rolls

        (doc, (js, css, rolls)) = runTemplate $ tabbed' @ts
        jsSrc = PP.displayT . PP.renderOneLine . renderJs $ js

tabbed' :: forall ts. TabbedSheet ts => Sheet ()
tabbed' = do
    div_ [class_ "sheet-container"] $ do
        div_ [class_ "sheet-tab-nav"] $ do
            field TabState
            field Editable

            forM_ (allTabs @ts) $ \tab -> do
                let action = "tab_" <> tabSlug tab
                    name' = toHtml (tabName tab)
                actionButton action name'
                tellJs $ tabAction (tabSlug tab)
            
            actionButton "ToggleEditable" "Toggle Edit Mode"
            tellJs editToggleAction
            tellCss (tabStyles @ts)

        div_ [class_ "sheet-main"] $ do
            field TabState
            field Editable

            sheetGlobal @ts

            forM_ (allTabs @ts) $ \tab -> do
                let tabClass = "sheet-tab-" <> tabSlug tab
                    roClasses = tabClass <> " " <> roTabClass
                    rwClasses = tabClass <> " " <> rwTabClass

                    tabR = tabBody tab
                div_ [class_ roClasses] $ embedModalTemplate ReadOnly  tabR
                div_ [class_ rwClasses] $ tagsOnly $ embedModalTemplate ReadWrite tabR
    where
        roTabClass = "sheet-tab-ro"
        rwTabClass = "sheet-tab-rw"

        tabAction :: T.Text -> JStat
        tabAction t =
            onEvent [Clicked $ "tab_" <> t] $ \_ ->
                setAttr TabState (toJExpr t)

        invertEditable :: JExpr -> JExpr
        invertEditable v = [jmacroE| !(`(e)` === 'true' || `(e)` === true) |]
            where
                e = getValueAtField Editable v

        editToggleAction :: JStat
        editToggleAction =
            onEvent [Clicked "ToggleEditable"] $ \_ ->
                getAttrs [Editable] $ \v ->
                    setAttr Editable (invertEditable v)
