{-# OPTIONS_ghc -Wno-partial-type-signatures #-}
module Renaissance.RollTemplates.WeaponDamage ( weaponDamageRollTemplate ) where

import Roll20.RollTemplate
import Roll20.Types

import Lucid

weaponDamageRollTemplate :: Tagged "WeaponDamage" _
weaponDamageRollTemplate = tag weaponDamageRollTemplate'

weaponDamageRollTemplate' :: Arg "CharacterName" -> Arg "WeaponName" -> Arg "WeaponDamage" -> Arg "WeaponAP" -> RollTemplate ()
weaponDamageRollTemplate' charName weaponName weaponDmg weaponAP = do
    div_ [class_ "sheet-roll-header"] $ arg charName

    div_ [class_ "sheet-roll-name"] $ arg weaponName

    div_ [class_ "sheet-roll-value"] $ do
        span_ [class_ "sheet-roll-result"] $ arg weaponDmg
        ", AP "
        span_ [class_ "sheet-roll-target"] $ arg weaponAP
