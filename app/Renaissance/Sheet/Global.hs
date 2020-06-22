module Renaissance.Sheet.Global ( global ) where

import Roll20.Field
import Roll20.RollTemplate ( rollTemplate )

import Renaissance.Fields.Global
import Renaissance.RollTemplates.SkillLike
import Renaissance.RollTemplates.Styles
import Renaissance.Sheet.Global.Styles

import Lucid

global :: Sheet ()
global = do
    tellCss globalStyles
    tellCss rollTemplateStyles
    tellRollTemplate $ rollTemplate skillLikeRollTemplate

    div_ [class_ "sheet-header"] $ do
        field ShortCharacterName
            `with` [placeholder_ "Name"]
        field ShortDescription
            `with` [placeholder_ "Short Description"]
