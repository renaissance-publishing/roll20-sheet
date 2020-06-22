module Renaissance.Sheet.Skills.ResetSkills ( resetSkills ) where

import qualified Data.Text as T
import Language.Javascript.JMacro
import Roll20.SheetWorker

import Renaissance.Fields.Global
import Renaissance.Fields.Skills

--

data Skill = Skill
           { skillName :: T.Text
           , skillApt :: Aptitude
           , skillIsProficiency :: Bool
           }
           deriving (Show, Eq)

defaultSkills :: [Skill]
defaultSkills = [ Skill "Animal Handling" SAV False
                , Skill "Art"             INT False
                , Skill "Athletics"       SOM True
                , Skill "Control"         WIL False
                , Skill "Craft"           COG False
                , Skill "Deception"       SAV False
                , Skill "Disable Device"  COO False
                , Skill "Disguise"        INT False
                , Skill "Fray"            COO False
                , Skill "Intimidate"      SAV False
                , Skill "Knowledge"       COG False
                , Skill "Medicine"        COG False
                , Skill "Melee Weapons"   SOM True
                , Skill "Perception"      SAV False
                , Skill "Pole-Arms"       SOM True
                , Skill "Profession"      COG False
                , Skill "Protocol"        SAV False
                , Skill "Ranged Weapons"  COO True
                , Skill "Read"            SAV False
                , Skill "Research"        COG False
                , Skill "Ride"            COO False
                , Skill "Search"          INT False
                , Skill "Sleight of Hand" COO False
                , Skill "Soothe"          SAV False
                , Skill "Spellcraft"      COG False
                , Skill "Survival"        INT False
                , Skill "Stealth"         COO False
                , Skill "Unarmed Combat"  SOM True
                ]

resetSkills' :: JStat
resetSkills' = clearSkills <> setDefaultSkills
    where
        clearSkills = withEachSectionId @SkillField (removeSectionById @SkillField)
        setDefaultSkills = foldMap setSkill defaultSkills

        setSkill (Skill name apt isProf) = withNewSectionId $ setAttrs' skillAttrs
            where
                skillAttrs = [ (SkillName, toJExpr name)
                             , (SkillAptitude, toJExpr $ show apt)
                             , (SkillIsProficiency, toJExpr isProf')
                             ]
                isProf' :: String
                isProf' = if isProf then "on" else "off"

resetSkills :: JStat
resetSkills = onEvent [Clicked "ResetSkills"] $ \_ -> resetSkills'
