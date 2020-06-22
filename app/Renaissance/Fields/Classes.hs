module Renaissance.Fields.Classes ( ClassField(..) ) where

import Roll20.Field

data ClassField = ClassName
                | ClassDescription
                deriving (Show, Eq)

instance RepeatingSection ClassField "classes" where
    repeatingFieldType ClassName        = TextF
    repeatingFieldType ClassDescription = TextareaF
