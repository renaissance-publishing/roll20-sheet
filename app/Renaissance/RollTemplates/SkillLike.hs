{-# OPTIONS_ghc -Wno-partial-type-signatures #-}
module Renaissance.RollTemplates.SkillLike ( skillLikeRollTemplate, woundTraumaModifier, epStyleDice ) where

import Roll20.RollTemplate
import Roll20.Types
import Renaissance.Fields.Global

import Lucid

skillLikeRollTemplate :: Tagged "SkillLike" _
skillLikeRollTemplate = tag skillLikeRollTemplate'

skillLikeRollTemplate' :: Arg "CharacterName" -> Arg "RollName" -> Arg "RollValue" -> Arg "RollTarget" -> RollTemplate ()
skillLikeRollTemplate' charName rollName rollVal rollTarget = do
    div_ [class_ "sheet-roll-header"] $ arg charName

    div_ [class_ "sheet-roll-name"] $ arg rollName

    div_ [class_ "sheet-roll-value"] $ do
        span_ [class_ "sheet-roll-result"] $ arg rollVal
        " vs "
        span_ [class_ "sheet-roll-target"] $ arg rollTarget

    div_ [class_ "sheet-roll-result-type"] $ do
        argLessThanOrEqual rollVal rollTarget $ do
            span_ [class_ "sheet-roll-success"] $ do
                argIsCrit rollVal "Critical Success!"
                argIsNotCrit rollVal "Success"
            span_ [class_ "sheet-roll-margin"] $ do
                "Margin: "
                arg rollVal
        argGreaterThan rollVal rollTarget $ do
            span_ [class_ "sheet-roll-failure"] $ do
                argIsCrit rollVal "Critical Failure!"
                argIsNotCrit rollVal "Failure"
            span_ [class_ "sheet-roll-margin"] $ do
                "Margin: "
                arg rollVal
                " - "
                arg rollTarget

woundTraumaModifier :: Roll
woundTraumaModifier = Max [0, 10 * woundModifier] + Max [0, 10 * traumaModifier]
    where
        woundModifier = Comment (Field $ Derived CurrentWounds) "Current Wounds"
                      - Comment (Field $ Derived IgnoredWounds) "Ignored Wounds"

        traumaModifier = Comment (Field $ Derived CurrentTrauma) "Current Trauma"
                       - Comment (Field $ Derived IgnoredTrauma) "Ignored Trauma"

epStyleDice :: Roll
epStyleDice = "[[1d100cs100cs89cs78cs67cs56cs45cs34cs23cs12cs1-1]]"
