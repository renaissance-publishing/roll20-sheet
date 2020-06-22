module Renaissance.Fields.Items ( ItemField(..) ) where

import Language.Javascript.JMacro

import Roll20.Field
import Roll20.SheetWorker
import Renaissance.Fields.Global

data ItemField = ItemName
               | ItemIsWeapon
               | ItemWeaponDamageDice
               | ItemWeaponDamageModifier
               | ItemWeaponReceivesDB
               | ItemWeaponEffectiveDB
               | ItemWeaponArmorPenetration
               | ItemDescription
               deriving (Show, Eq)

instance RepeatingSection ItemField "items" where
    repeatingFieldType ItemName                   = TextF
    repeatingFieldType ItemIsWeapon               = CheckboxF
    repeatingFieldType ItemWeaponDamageDice       = NumberF
    repeatingFieldType ItemWeaponDamageModifier   = NumberF
    repeatingFieldType ItemWeaponReceivesDB       = CheckboxF
    repeatingFieldType ItemWeaponEffectiveDB      = NumberF
    repeatingFieldType ItemWeaponArmorPenetration = NumberF
    repeatingFieldType ItemDescription            = TextareaF

    repeatingFieldWorker dst@ItemWeaponEffectiveDB = Just worker
        where
            worker = autocalculateRepeating dst ItemWeaponReceivesDB (Derived DB) go
            go hasDB db = [jmacroE| `(hasDB)` === "on" ? `(db)` : 0 |]
    repeatingFieldWorker _ = Nothing
