{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.Sheet.Aptitudes.Styles
    ( aptitudeStyles ) where

import qualified Data.Text as T

import Renaissance.Fields.Global

import qualified Roll20.CSS.Elements as E
import Roll20.CSS

aptitudeStyles :: Css
aptitudeStyles = do
    ".sheet-aptitude-table" ? do
        fieldType NumberF ?
            textAlign center
        E.span ?
            textAlign center

    wtStyles
    statTrackerStyles
    commonRollsStyles

--

styleForLevel :: Int -> Css
styleForLevel n =
    forM_ [n + 1 .. 5] $ \m ->
        field WealthLevel # valueEq (tshow n) |~ rowSel m ? display none
    where
        rowSel m = E.tr # byClass ("sheet-wealth-" <> T.pack (show m))

wtStyles :: Css
wtStyles = mapM_ styleForLevel [0..5]

--

statTrackerStyles :: Css
statTrackerStyles = ".sheet-stat-tracker" ? do
    textAlign center

    fieldType NumberF ? do
        important $ do
            display inlineBlock
            width (px 40)
        textAlign center

--

commonRollsStyles :: Css
commonRollsStyles =
    ".sheet-common-rolls" ? E.tr ? E.td ?
        sym padding (em 0.5)
