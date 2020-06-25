{-# OPTIONS_ghc -Wno-orphans #-}
module Renaissance.Fields.Skills ( SkillField(..) ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Universe.Class ( universe )
import Language.Javascript.JMacro

import Roll20.SheetWorker
import Roll20.Field

import Renaissance.Fields.Global

--

data SkillField = SkillName
                | SkillIsProficiency
                | SkillProficiencies
                | SkillHasSpecialization
                | SkillSpecialization
                | SkillAptitude
                | SkillRanks
                | SkillTotal
                deriving (Show, Eq)

instance RepeatingSection SkillField where
    type RepeatingSectionName SkillField = "skills"

    repeatingFieldType SkillName = TextF
    repeatingFieldType SkillIsProficiency = CheckboxF
    repeatingFieldType SkillProficiencies = TextF
    repeatingFieldType SkillHasSpecialization = CheckboxF
    repeatingFieldType SkillSpecialization    = TextF
    repeatingFieldType SkillAptitude = SelectF aptitudeNames
        where aptitudeNames = T.pack . show <$> universe @Aptitude
    repeatingFieldType _ = NumberF

    repeatingFieldWorker SkillTotal = Just worker
        where
            worker = autocalculateRepeating SkillTotal attrs aptTotals go

            go apt ranks cog coo int sav som wil = aptVal + ranks
                where
                    aptVal = IdxExpr (toJExpr aptMap) apt
                    aptMap = M.fromList $
                        [ (show COG, cog)
                        , (show COO, coo)
                        , (show INT, int)
                        , (show SAV, sav)
                        , (show SOM, som)
                        , (show WIL, wil)
                        ]

            attrs = (SkillAptitude, SkillRanks)

            aptTotals = ( Apt COG Total
                        , Apt COO Total
                        , Apt INT Total
                        , Apt SAV Total
                        , Apt SOM Total
                        , Apt WIL Total
                        )
    repeatingFieldWorker _ = Nothing
