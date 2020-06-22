{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.Sheet.Skills.Styles ( skillsStyles ) where

import Renaissance.Fields.Skills

import qualified Roll20.CSS.Elements as E
import Roll20.CSS

skillsStyles :: Css
skillsStyles = do
    ".sheet-skill-table" ? do
        ".sheet-fake-header" <? do
            display grid
            gridTemplateColumns [pct 50, pct 16.6, pct 16.6, auto]

        ".repitem" ? do
            display grid
            gridTemplateColumns [pct 50, pct 16.6, pct 16.6, auto]

            rollButton ?
                width (pct 100)

        repField SkillAptitude ?
            textAlign center

        repField SkillIsProficiency # valueNotEq "on" |~ repField SkillProficiencies ?
            important (display none)

        repField SkillHasSpecialization # valueNotEq "on" |~ repField SkillSpecialization ?
            important (display none)

        ".sheet-skill-subfield" ?
            important (fontSize $ em 1.3)

        fieldType (SelectF []) ? do
            width auto
            sym margin nil

    tabMode ReadOnly ? ".sheet-skill-table" ? do
        repField SkillIsProficiency # valueEq "on" |~
            E.button # not ".sheet-skill-roll-proficiency" ?
                display none

        repField SkillIsProficiency # valueNotEq "on" |~
            ".sheet-skill-roll-proficiency" ?
                display none

        repField SkillHasSpecialization # valueEq "on" |~
            E.button # not ".sheet-skill-roll-specialization" ?
                display none

        repField SkillHasSpecialization # valueNotEq "on" |~
            ".sheet-skill-roll-specialization" ?
                display none


    actionButton "ResetSkills" ? do
        fontSize (em 1.3)
        marginTop (px 10)

    let langRowStyles = do
            display grid
            gridTemplateColumns [pct 80, auto]

    ".sheet-language-table" ? do
        ".sheet-fake-header" <? langRowStyles
        ".repitem" ? langRowStyles
