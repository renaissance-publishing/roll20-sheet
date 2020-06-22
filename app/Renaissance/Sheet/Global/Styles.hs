{-# LANGUAGE NoImplicitPrelude #-}
module Renaissance.Sheet.Global.Styles ( globalStyles, plainNameField ) where

import Renaissance.Fields.Global

import qualified Roll20.CSS.Colors as C
import qualified Roll20.CSS.Elements as E
import Roll20.CSS

typography :: Css
typography = do
    fontFamily ["IM FELL DW Pica"] [serif]
    color C.black

globalStyles :: Css
globalStyles = do
    importUrl "\"https://fonts.googleapis.com/css?family=IM+Fell+DW+Pica\""

    forM_ [star, star # before, star # before] $ \sel ->
        sel ? boxSizing borderBox

    forM_ ["::-webkit-inner-spin-button", "::-webkit-outer-spin-button"] $ \sb ->
        fieldType NumberF # sb ? do
            "-webkit-appearance" -: "none"
            sym margin nil

    ".sheet-container" ? do
        minWidth $ px 700
        height $ vh 100

        display grid
        gridTemplateColumns [px 240, auto]

        typography

        forM_ [E.h1, E.h2, E.h3] $ \h -> h ? do
            marginBottom (px 10)

            not (star # firstChild) &
                marginTop (px 20)

        ".sheet-ro-textarea" ? do
            userSelect selectText
            whiteSpace preWrap

        forM_ [ E.input, E.button, E.textarea ] $ \sel -> sel ? do
            typography
            "line-height" -: "unset"

            fieldReadOnly & do
                "background" -: "none"
                cursor cursorDefault

        fieldType CheckboxF ?
            sym2 margin nil (px 5)

        fieldType TextareaF ? do
            width (pct 100)
            height (inches 3)
            marginTop (em 0.25)
            "resize" -: "none"

        ".sheet-main" ? E.button ? do
            before &
                content none
            sym padding nil
            sym margin nil

            "background" -: "none"
            "border" -: "none"
            "box-shadow" -: "none"
            textDecoration underline
            textDecorationStyle dotted

        tableStyles

    rollButton ? do
        "background" -: "none"
        borderColor (other unset)

        color inherit
        important $ fontSize (em 1)
        fontWeight bold
        "text-shadow" -: "none"

    let headerHeight = px 100
    ".sheet-header" ? do
        height headerHeight

        E.input ? do
            display block
            width (pct 100)
            textAlign center
            "border" -: "none"

        field ShortCharacterName ?
            fontSize (px 36)

        field ShortDescription ?
            fontSize (px 18)

    ".sheet-tab-nav" ?
        marginTop headerHeight

    ".sheet-center" ? do
        display flex
        justifyContent center
        alignItems center

tableStyles :: Css
tableStyles = do
    E.table ? do
        rootStyles
        "empty-cells" -: "show"

        E.thead ? E.tr # onlyChild ? E.th ?
            rowBorder

        E.tr ? do
            forM_ [E.td, E.th] $ \c -> do
                notLast & c ? rowBorder
                c # notLast ? colBorder

        E.th ? headingStyles
        E.td ? cellStyles
    ".sheet-fake-table" ? do
        rootStyles

        E.span ? do
            display block

        ".sheet-fake-header" ? star <? rowBorder

        ".repitem" ? star # notLast <? colBorder
        ".sheet-fake-header" ? star # notLast <? colBorder

        ".sheet-fake-header" ? E.span ? headingStyles
        star # not ".sheet-fake-header" ? E.span ? cellStyles

    tabMode ReadOnly ? ".sheet-fake-table" ?
        ".repitem" # notLast ? star <? rowBorder

    tabMode ReadWrite ? ".sheet-fake-table" ?
        ".repitem" ? star <? rowBorder

    forM_ [TextF, NumberF] $ \ty ->
        ".sheet-center-inputs" ? fieldType ty ?
            textAlign center

    ".sheet-fixed-table" ? do
        "table-layout" -: "fixed"

    where
        rootStyles = do
            width (pct 100)
            borderCollapse collapse
            borderSpacing nil
            border solid (px 1) C.darkgrey

            forM_ [TextF, NumberF] $ \ty ->
                fieldType ty ? do
                    fieldStyles
                    onlyChild & height (pct 100)

            ".sheet-ro-select" ? fieldStyles

        fieldStyles = do
            display block
            width (pct 100)
            sym margin nil
            fontSize (em 1.5)
            "border" -: "none"

        colBorder = borderRight solid (px 1) C.darkgrey
        rowBorder = borderBottom solid (px 1) C.darkgrey

        commonCell = do
            sym margin nil

        headingStyles = do
            commonCell
            backgroundColor "#434343"
            color "#fefefe"
            sym padding (em 0.5)
            textAlign center

            forM_ [TextF, NumberF] $ \ty ->
                fieldType ty ? do
                    backgroundColor "#434343"
                    color "#fefefe"

        cellStyles = do
            commonCell
            sym2 padding nil (em 0.25)

        notLast = not $ star # lastChild

--

plainNameField :: Css
plainNameField = do
    display block
    width (pct 100)
    sym margin nil
    sym2 padding (px 4) nil
    important $ fontSize (em 1.5)
    "border" -: "none"
