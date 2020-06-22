{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.Sheet.Items.Styles ( itemsStyles ) where

import Renaissance.Sheet.Global.Styles ( plainNameField )

import Renaissance.Fields.Items

import qualified Roll20.CSS.Elements as E
import Roll20.CSS

itemsStyles :: Css
itemsStyles = do
    repField ItemName ?
        plainNameField

    tabMode ReadWrite ? repField ItemName ?
        marginBottom (px 10)

    repField ItemIsWeapon # valueNotEq "on" |~
        ".sheet-item-weapon" ?
            display none
    
    E.div # ("data-groupname" @= "repeating_items") ?
        E.button ?
            display block

    ".sheet-item-weapon-damage" ? do
        marginBottom (px 10)
        fontSize (em 1.2)

        E.input ?
            borderWidth nil
