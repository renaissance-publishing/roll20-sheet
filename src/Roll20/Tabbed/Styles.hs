{-# LANGUAGE NoImplicitPrelude #-}
module Roll20.Tabbed.Styles ( tabStyles ) where

import Roll20.Tabbed.Types

import Roll20.CSS
import qualified Roll20.CSS.Colors as C
import qualified Roll20.CSS.Elements as E

activeButton :: Css
activeButton = do
    borderLeftColor C.lightgreen
    backgroundColor "#efefef"

tabStyles :: forall ts. (TabbedSheet ts) => Css
tabStyles = do
    ".sheet-main" ? do
        width (pct 100)
        maxWidth (inches 8.5)
        "justify-self" -: "center"

        E.div # (not ".sheet-header") <? do
            display none
            sym2 padding nil (em 1)
    
    field Editable # valueEq "true" |~ tabMode ReadOnly ?
        important (display none)

    field Editable # valueEq "true" |~ actionButton "ToggleEditable" ?
        borderLeftColor C.red

    field Editable # valueEq "false" |~ tabMode ReadWrite ?
        important (display none)

    field Editable # valueEq "false" |~ actionButton "ToggleEditable" ?
        borderLeftColor C.lightgreen

    tabMode ReadOnly ? ".repcontrol" ?
        display none

    ".sheet-tab-nav" ? E.button ? do
        display block
        width (pct 100)
        sym margin nil
        sym padding (em 0.5)

        "background" -: "none"
        "border" -: "none"
        borderLeft solid (px 2) transparent
        textAlign $ alignSide sideCenter

        ":active" &
            color C.black
        
        (not $ star # firstOfType) &
            borderTop solid (px 2) C.black

    forM_ (allTabs @ts) $ \t -> do
        field TabState # valueEq (tabSlug t) |~ tabButton t ? activeButton
        field TabState # valueEq (tabSlug t) |~ tabContents t ? display block
    
    field TabState # valueEq "default" |~ tabButton firstTab ? activeButton
    field TabState # valueEq "default" |~ tabContents firstTab ? display block

    where
        (firstTab:_) = allTabs @ts

        tabButton t = actionButton $ "tab_" <> tabSlug t
        tabContents t = E.div # (byClass $ "sheet-tab-" <> tabSlug t)
