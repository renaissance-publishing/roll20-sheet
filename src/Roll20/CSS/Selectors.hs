module Roll20.CSS.Selectors
    ( actionButton, rollButton
    , field, repField
    , fieldType, FieldType(..)
    , fieldReadOnly
    , valueEq, valueNotEq
    , tabMode, SheetMode(..)
    ) where

import qualified Data.Text as T
import Prelude hiding ( not )

import Roll20.Field.Types hiding ( fieldType )

import Clay

actionButton :: T.Text -> Selector
actionButton named = button # ("type" @= "action") # ("name" @= rawName)
    where
        rawName = "act_" <> named
    
rollButton :: Selector
rollButton = button # ("type" @= "roll")

field :: (SheetSection f) => f -> Selector
field f = star # ("name" @= rawName)
    where
        rawName = "attr_" <> fieldName f

fieldType :: FieldType -> Selector
fieldType HiddenF = input # ("type" @= "hidden")
fieldType TextF = input # ("type" @= "text")
fieldType NumberF = input # ("type" @= "number")
fieldType CheckboxF = input # ("type" @= "checkbox")
fieldType TextareaF = textarea
fieldType (SelectF _) = select

fieldReadOnly :: Refinement
fieldReadOnly = readonly

repField :: (RepeatingSection r name) => r -> Selector
repField rf = star # ("name" @= rawName)
    where
        rawName = "attr_" <> repeatingFieldName rf

valueEq :: T.Text -> Refinement
valueEq = ("value" @=)

valueNotEq :: T.Text -> Refinement
valueNotEq v = not $ star # ("value" @= v)

--

tabMode :: SheetMode -> Selector
tabMode ReadOnly  = ".sheet-tab-ro"
tabMode ReadWrite = ".sheet-tab-rw"
