module Renaissance.Fields.Languages ( LanguageField(..) ) where

import Roll20.Field

data LanguageField = LanguageName
                   | LanguageRanks
                   deriving (Show, Eq)

instance RepeatingSection LanguageField where
    type RepeatingSectionName LanguageField = "languages"

    repeatingFieldType LanguageName  = TextF
    repeatingFieldType LanguageRanks = NumberF

    repeatingFieldDefault _ = Nothing
