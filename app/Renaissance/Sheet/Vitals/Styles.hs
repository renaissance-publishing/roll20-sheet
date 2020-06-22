{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.Sheet.Vitals.Styles ( vitalsStyles ) where

import Renaissance.Sheet.Global.Styles ( plainNameField )

import Renaissance.Fields.Milestones

import qualified Roll20.CSS.Elements as E
import Roll20.CSS

vitalsStyles :: Css
vitalsStyles = do
    ".sheet-motivations" ? E.input ? do
        textAlign center
        important (fontSize $ em 2)

    repField MilestoneName ? plainNameField
