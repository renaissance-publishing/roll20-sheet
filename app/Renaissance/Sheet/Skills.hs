module Renaissance.Sheet.Skills ( skills ) where

import Control.Monad ( when )
import qualified Data.Text as T

import Roll20.Field
import Roll20.RollTemplate

import Renaissance.Fields.Global
import Renaissance.Fields.Languages
import Renaissance.Fields.Skills
import Renaissance.RollTemplates.SkillLike
import Renaissance.Sheet.Skills.ResetSkills
import Renaissance.Sheet.Skills.Styles

import Lucid

skills :: SheetR ()
skills = do
    editable <- isEditable
    tellCss skillsStyles
    tellJs resetSkills

    h2_ "Skills"
    div_ [class_ "sheet-fake-table sheet-center-inputs sheet-skill-table"] $ do
        div_ [class_ "sheet-fake-header"] $ do
            span_ "Skill"
            span_ "Aptitude"
            span_ "Ranks"
            span_ "Target"
        repeating @SkillField $ do
            skillNameR <- repFieldRoll SkillName
            skillTotalR <- repFieldRoll SkillTotal

            let rollForSkill = rollWithTemplate skillLikeRollTemplate
                totalR = Comment skillTotalR "Skill Target"

                specQuery = flip Comment "Specialization bonus" $
                    EnumQuery "Specialization applies?" [("No", 0), ("Yes", 10)]
                profQuery = flip Comment "Proficiency penalty" $
                    EnumQuery "Proficiency applies?" [("Yes", 0), ("No", -20)]

                rollForSkill' r =
                    rollForSkill
                        (Field ShortCharacterName)
                        skillNameR
                        epStyleDice
                        (r - woundTraumaModifier + modifier)

                normalSkill = totalR
                normalSkillSpec = normalSkill + specQuery
                profSkill = totalR + profQuery
                profSkillSpec = profSkill + specQuery

                withClasses cs = flip with [class_ $ T.intercalate " " cs]
                specClass = "sheet-skill-roll-specialization"
                profClass = "sheet-skill-roll-proficiency"

            div_ $ do
                hiddenRepField SkillIsProficiency
                hiddenRepField SkillHasSpecialization

                let skillName = repField' SkillName `with` [placeholder_ "Skill"]
                case editable of
                    ReadOnly -> do
                        rollButton (rollForSkill' normalSkill) skillName

                        withClasses [specClass] $
                            rollButton (rollForSkill' normalSkillSpec) skillName

                        withClasses [profClass] $
                            rollButton (rollForSkill' profSkill) skillName

                        withClasses [profClass, specClass] $
                            rollButton (rollForSkill' profSkillSpec) skillName

                    ReadWrite -> do
                        skillName
                        label_ $ do
                            repField' SkillIsProficiency
                            "Is Proficiency"

                repField' SkillProficiencies `with` [placeholder_ "Proficiencies", class_ "sheet-skill-subfield"]

                when (editable == ReadWrite) $
                    label_ $ do
                        repField' SkillHasSpecialization
                        "Has Specialization"

                repField' SkillSpecialization
                        `with` [placeholder_ "Specialization", class_ "sheet-skill-subfield"]

            div_ [class_ "sheet-center"] $
                repField' SkillAptitude

            div_ [class_ "sheet-center"] $
                repField' SkillRanks `with` [placeholder_ "Ranks"]

            div_ [class_ "sheet-center"] $
                repField' SkillTotal `with` [placeholder_ "Total"]

    when (editable == ReadWrite) $
        actionButton "ResetSkills" "Reset Skills"

    h2_ "Languages"
    div_ [class_ "sheet-fake-table sheet-center-inputs sheet-language-table"] $ do
        div_ [class_ "sheet-fake-header"] $ do
            span_ "Language"
            span_ "Ranks"
        repeating @LanguageField $ do
            div_ $
                repField' LanguageName
            div_ $
                repField' LanguageRanks
