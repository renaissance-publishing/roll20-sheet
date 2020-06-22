module Renaissance.Sheet.ClassesTraitsPowers ( classesTraitsPowers ) where

import Roll20.Field
import Renaissance.Sheet.ClassesTraitsPowers.Styles ( ctpStyles )
import Renaissance.Fields.Classes
import Renaissance.Fields.Traits
import Renaissance.Fields.Powers

import Lucid

classesTraitsPowers :: SheetR ()
classesTraitsPowers = do
    tellCss ctpStyles

    h2_ "Classes"
    repeating @ClassField $ do
        repField' ClassName `with` [placeholder_ "Name"]
        repField' ClassDescription `with` [placeholder_ "Description"]

    h2_ "Traits"
    repeating @TraitField $ do
        repField' TraitName `with` [placeholder_ "Name"]
        repField' TraitDescription `with` [placeholder_ "Description"]

    h2_ "Powers"
    repeating @PowerField $ do
        repField' PowerName `with` [placeholder_ "Name"]
        div_ $ do
            "Action: "
            repField' PowerAction `with` [placeholder_ "Action"]
        repField' PowerDescription `with` [placeholder_ "Description"]
