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

-- | A selector that matches action buttons based on the name of their action.
--
--   Note that this function handles prepending @act_@, and therefore it
--   should not be used in the argument.
actionButton :: T.Text -> Selector
actionButton named = button # ("type" @= "action") # ("name" @= rawName)
    where
        rawName = "act_" <> named

-- | A selector that matches roll buttons.
rollButton :: Selector
rollButton = button # ("type" @= "roll")

-- | A selector for a particular global field by name.
field :: (SheetSection f) => f -> Selector
field f = star # ("name" @= rawName)
    where
        rawName = "attr_" <> fieldName f

-- | A selector for fields of a given 'FieldType'.
fieldType :: FieldType -> Selector
fieldType HiddenF = input # ("type" @= "hidden")
fieldType TextF = input # ("type" @= "text")
fieldType NumberF = input # ("type" @= "number")
fieldType CheckboxF = input # ("type" @= "checkbox")
fieldType TextareaF = textarea
fieldType (SelectF _) = select

-- | Matches (@\<input>@-based) fields that are read-only.
fieldReadOnly :: Refinement
fieldReadOnly = readonly

-- | A selector for a particular repeating field by name.
repField :: (RepeatingSection r) => r -> Selector
repField rf = star # ("name" @= rawName)
    where
        rawName = "attr_" <> repeatingFieldName rf

-- | Matches fields where the value is equal to the provided value.
valueEq :: T.Text -> Refinement
valueEq = ("value" @=)

-- | Matches fields where the value is not equal to the provided value.
valueNotEq :: T.Text -> Refinement
valueNotEq v = not $ star # ("value" @= v)

--

-- | A selector that targets a version of a tab based on the provided
--   'SheetMode'.
tabMode :: SheetMode -> Selector
tabMode ReadOnly  = ".sheet-tab-ro"
tabMode ReadWrite = ".sheet-tab-rw"
