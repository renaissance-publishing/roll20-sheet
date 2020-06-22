{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.RollTemplates.Styles ( rollTemplateStyles ) where

import qualified Roll20.CSS.Colors as C
import qualified Roll20.CSS.Elements as E
import Roll20.CSS

rollTemplateStyles :: Css
rollTemplateStyles = do
    ".sheet-rolltemplate-Initiative" <? rollStyles
    ".sheet-rolltemplate-SkillLike" <? rollStyles

rollStyles :: Css
rollStyles = do
    fontFamily ["IM FELL DW Pica"] [serif]

    border solid (px 1) C.darkgrey

    E.div # not (star # lastChild) ? do
        borderBottom solid (px 1) C.darkgrey

    E.div ?
        sym padding (px 5)

    ".sheet-roll-header" ? do
        textAlign center
        background ("#434343" :: Color)
        color "#FEFEFE"
        fontSize (em 1.3)

    ".sheet-roll-name" ? do
        background ("#FEFEFE" :: Color)
        fontSize (em 1.2)

    ".sheet-roll-value" ? do
        background ("#FEFEFE" :: Color)
        textAlign center

        ".sheet-roll-result" ?
            fontSize (em 1.3)

        ".sheet-roll-target" ?
            fontSize (em 1.3)

    ".sheet-roll-result-type" ? do
        background ("#FEFEFE" :: Color)
        display flex
        justifyContent spaceBetween
        fontSize (em 1.2)

    ".sheet-roll-success" |~ ".sheet-roll-margin" ? ".inlinerollresult" ?
        color "#8BC34A"

    ".sheet-roll-failure" |~ ".sheet-roll-margin" ? ".inlinerollresult" ?
        color "#F44336"

    ".inlinerollresult" ? do
        borderWidth nil
        backgroundColor transparent

        forM_ [".fullcrit", ".fullfail", ".importantroll"] $ \r -> r &
            borderWidth nil
