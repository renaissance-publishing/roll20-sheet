module Renaissance.Sheet.Items ( itemsTab ) where

import Control.Monad ( when )

import Roll20.Field
import Roll20.RollTemplate

import Renaissance.Fields.Global
import Renaissance.Fields.Items
import Renaissance.RollTemplates.WeaponDamage
import Renaissance.Sheet.Items.Styles

import Lucid

itemsTab :: SheetR ()
itemsTab = do
    editable <- isEditable
    tellCss itemsStyles
    tellRollTemplate $ rollTemplate weaponDamageRollTemplate

    h2_ "Items"
    repeating @ItemField $ do
        weaponName     <- repFieldRoll ItemName
        damageDice     <- repFieldRoll ItemWeaponDamageDice
        damageModifier <- repFieldRoll ItemWeaponDamageModifier
        damageBonus    <- repFieldRoll ItemWeaponEffectiveDB
        weaponAP       <- repFieldRoll ItemWeaponArmorPenetration

        let itemRoll =
                rollWithTemplate
                    weaponDamageRollTemplate
                    (Field ShortCharacterName)
                    weaponName
                    ( Comment (d10 damageDice + damageModifier) "Damage Roll"
                    + Comment damageBonus "DB"
                    )
                    weaponAP

            itemName = repField' ItemName `with` [placeholder_ "Name"]
        
        hiddenRepField ItemIsWeapon

        when (editable == ReadOnly) $
            rollButton itemRoll itemName
        
        itemName

        when (editable == ReadWrite) $ do

            label_ $ do
                repField' ItemIsWeapon
                "Weapon"

            div_ [class_ "sheet-item-weapon"] $ do
                div_ [class_ "sheet-item-weapon-damage"] $ do
                    "Damage: "
                    repField' ItemWeaponDamageDice
                    "d10 + "
                    repField' ItemWeaponDamageModifier
                    ", AP: "
                    repField' ItemWeaponArmorPenetration

                label_ $ do
                    repField' ItemWeaponReceivesDB
                    "Receives DB"

        repField' ItemDescription
