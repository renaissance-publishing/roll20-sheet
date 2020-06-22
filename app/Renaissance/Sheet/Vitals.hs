module Renaissance.Sheet.Vitals ( vitals ) where

import Control.Monad ( forM_ )
import Data.Universe.Class ( Finite(..) )

import Roll20.Field

import Renaissance.Fields.Global
import Renaissance.Fields.Milestones
import Renaissance.Sheet.Vitals.Styles

import Lucid

vitals :: SheetR ()
vitals = do
    tellCss vitalsStyles

    h2_ "Vital Statistics"
    table_ [class_ "sheet-vital-table"] $
        tbody_ $ do
            let vitalStats = universeF @VitalStat
            forM_ vitalStats $ \vital -> tr_ $ do
                th_ [scope_ "row"] (toHtml vital)
                td_ $ field' (Vital vital)

    h2_ "Description"
    field' Description

    h2_ "Background"
    field' DetailedBackground

    h2_ "Motivations"
    table_ [class_ "sheet-motivations"] $ tbody_ $
        forM_ [1, 2, 3] $ \n -> tr_ $ do
            let ty_attrs   = [maxlength_ "1", placeholder_ "Type"]
                desc_attrs = [placeholder_ "Description"]
            td_ $ field' (MotivationType n) `with` ty_attrs
            th_ $ field' (MotivationDesc n) `with` desc_attrs

    h2_ "Milestones"
    repeating @MilestoneField $ do
        repField' MilestoneName
            `with` [placeholder_ "Name"]
        repField' MilestoneDescription
            `with` [placeholder_ "Description"]
