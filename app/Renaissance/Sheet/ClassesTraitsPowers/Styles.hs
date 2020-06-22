{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.Sheet.ClassesTraitsPowers.Styles ( ctpStyles ) where

import Renaissance.Sheet.Global.Styles ( plainNameField )
import Renaissance.Fields.Classes
import Renaissance.Fields.Traits
import Renaissance.Fields.Powers

import Roll20.CSS

ctpStyles :: Css
ctpStyles = do
    repField ClassName ? plainNameField
    repField TraitName ? plainNameField
    repField PowerName ? plainNameField

    repField PowerAction ? do
        display inlineBlock
        sym margin nil
        "border" -: "none"
