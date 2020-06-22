module Renaissance.Sheet.Aptitudes ( aptitudesAndDerived ) where

import Control.Monad ( forM_, zipWithM_, when )
import qualified Data.Text as T
import Data.Universe.Class ( universe )

import Roll20.Field
import Roll20.RollTemplate ( rollTemplate )

import Renaissance.Fields.Global
import Renaissance.RollTemplates.INIT
import Renaissance.Sheet.Aptitudes.SpecialRolls
import Renaissance.Sheet.Aptitudes.Styles

import Lucid

--

luckTracker :: SheetR ()
luckTracker = do
    editable <- isEditable
    th_ $
        case editable of
            ReadOnly -> rollButton luckRoll "Luck"
            ReadWrite -> "Luck"
    td_ [class_ "sheet-stat-tracker"] $ do
        field' LuckCurrent
        "/"
        field' LuckTotal

--

wealthTracker :: SheetR ()
wealthTracker = do
    tr_ $ do
        th_ [rowspan_ "6"] "Wealth"
        th_ "Maximum Wealth"
        td_ [colspan_ "2"] $ field' WealthLevel

    hiddenField WealthLevel

    forM_ [1..5] wealthRow

wealthRow :: Int -> SheetR ()
wealthRow n = tr_ [class_ rowClass]$ do
    th_ $ toHtml n'
    td_ [colspan_ "2", class_ "sheet-stat-tracker"] $ do
        field' $ WealthCurrent n
        "/"
        field' $ WealthTotal n

    where
        rowClass = "sheet-wealth-" <> n'
        n' = T.pack (show n)

--

aptitudesAndDerived :: SheetR ()
aptitudesAndDerived = do
    tellRollTemplate (rollTemplate initRollTemplate)
    tellCss aptitudeStyles

    h2_ "Aptitudes"
    table_ [class_ "sheet-aptitude-table"] $ do
        let aptitudes = universe @Aptitude
            aptitudeRows = universe @AptitudeRow

        thead_ $
            tr_ $ do
                th_ ""
                forM_ aptitudes $ \apt -> do
                    th_ [scope_ "col"] (toHtml apt)

        tbody_ $ do
            forM_ aptitudeRows $ \row ->
                tr_ $ do
                    th_ [scope_ "row"] (toHtml row)
                    forM_ aptitudes $ \apt -> td_ $
                        field' (Apt apt row)

    h2_ "Derived Statistics"
    table_ [class_ "sheet-center-inputs sheet-fixed-table"] $
        tbody_ $ do
            zipWithM_ mkRow derivedCol1' derivedCol2'

            tr_ $ do
                th_ "Movement"
                td_ [colspan_ "3"] $
                    field' Movement

            tr_ $ do
                th_ "Senses"
                td_ [colspan_ "3"] $
                    field' Senses

            wealthTracker

    editable <- isEditable
    when (editable == ReadOnly) $ do
        h2_ "Common Rolls"
        table_ [class_ "sheet-common-rolls"] $
            tbody_ $
                mapM_ specialRollHtml specialRolls
    where
        mkRow l r = tr_ (l <> r)

        derivedCol1, derivedCol2 :: [DerivedStat]
        derivedCol1 = [CurrentWounds, IgnoredWounds, DUR, WT, DR, DB, STR]
        derivedCol2 = [CurrentTrauma, IgnoredTrauma, LUC, TT, IR, INIT]

        derivedCol1' = fmap derivedRow derivedCol1
        derivedCol2' = fmap derivedRow derivedCol2 ++ [luckTracker]

        derivedRow :: DerivedStat -> SheetR ()
        derivedRow INIT = do
            editable <- isEditable
            th_ $
                case editable of
                    ReadOnly ->
                        rollButton initRoll "INIT"
                    ReadWrite -> "INIT"
            td_ $ field' (Derived INIT)
        derivedRow d = do
            th_ $ toHtml d
            td_ $ field' (Derived d)
