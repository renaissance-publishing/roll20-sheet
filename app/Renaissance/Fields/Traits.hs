module Renaissance.Fields.Traits ( TraitField(..) ) where

import Roll20.Field

data TraitField = TraitName
                | TraitDescription
                deriving (Show, Eq)

instance RepeatingSection TraitField where
    type RepeatingSectionName TraitField = "traits"

    repeatingFieldType TraitName        = TextF
    repeatingFieldType TraitDescription = TextareaF
