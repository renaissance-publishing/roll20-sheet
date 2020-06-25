module Renaissance.Fields.Powers ( PowerField(..) ) where

import Roll20.Field

data PowerField = PowerName
                | PowerAction
                | PowerDescription
                deriving (Show, Eq)

instance RepeatingSection PowerField where
    type RepeatingSectionName PowerField = "powers"

    repeatingFieldType PowerName        = TextF
    repeatingFieldType PowerAction      = TextF
    repeatingFieldType PowerDescription = TextareaF
