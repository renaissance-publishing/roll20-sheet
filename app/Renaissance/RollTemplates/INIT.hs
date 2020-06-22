{-# OPTIONS_ghc -Wno-partial-type-signatures #-}
module Renaissance.RollTemplates.INIT ( initRollTemplate, initRoll ) where

import Roll20.RollTemplate
import Roll20.Types

import Renaissance.Fields.Global

import Lucid

initRollTemplate :: Tagged "Initiative" _
initRollTemplate = tag initRollTemplate'

initRollTemplate' :: Arg "CharacterName" -> Arg "Initiative" -> RollTemplate ()
initRollTemplate' name initVal = do
    div_ [class_ "sheet-roll-header"] $ arg name
    div_ [class_ "sheet-roll-name"] "Initiative"
    div_ [class_ "sheet-roll-value"] $
        span_ [class_ "sheet-roll-result"] $ arg initVal

initRoll :: Roll
initRoll = rollWithTemplate initRollTemplate (Field ShortCharacterName) roll
    where
        initMod = Comment (Field $ Derived INIT) "INIT mod"
        roll = ToTracker $ d10 1 + initMod + modifier
