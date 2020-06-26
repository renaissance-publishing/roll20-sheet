module Roll20.Field.Types
    ( FieldType(..), fieldTypeAttrs
    , SheetMode(..)
    , SheetSection(..)
    , RepeatingSection(..)
    , repeatingSectionName, repeatingSectionName'
    ) where

import Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal )
import Language.Javascript.JMacro ( JStat )
import qualified Lucid as L

import Roll20.Types

-- | This type represents the different types of field that can be presented
--   to the user. There is no actual type-safety in the Roll20 backend, and it
--   would be excessively difficult to emulate it within the library, so this
--   purely exists to control how a field is presented to the user.
data FieldType = HiddenF          -- ^ In HTML: @\<input type="hidden">@
               | TextF            -- ^ In HTML: @\<input type="text">@
               | NumberF          -- ^ In HTML: @\<input type="number">@
               | CheckboxF        -- ^ In HTML: @\<input type="checkbox">@
               | TextareaF        -- ^ In HTML: @\<textarea>@
               | SelectF [T.Text] -- ^ In HTML: @\<select>@, with each element
                                  --   of the list used to create an 
                                  --   @\<option>@
               deriving (Eq, Show)

fieldTypeAttrs :: FieldType -> [L.Attribute]
fieldTypeAttrs HiddenF     = [L.type_ "hidden"]
fieldTypeAttrs TextF       = [L.type_ "text", L.spellcheck_ "false"]
fieldTypeAttrs NumberF     = [L.type_ "number"]
fieldTypeAttrs CheckboxF   = [L.type_ "checkbox"]
fieldTypeAttrs TextareaF   = [L.spellcheck_ "false"]
fieldTypeAttrs (SelectF _) = []

-- | A type with an instance of this class represents a set of fields @f@ that
--   exist within the global scope of the Roll20 sheet.
--
--   It is possible to have fields from more than one instance of
--   @SheetSection@ within a single sheet, but in practice, it can lead to
--   difficulties in utilizing the functionality of the "Roll20.SheetWorker"
--   module.
class SheetSection f where
    fieldName :: f -> T.Text
    -- ^ The (internal) name for a given field. This is the name that will be
    --   used inside of the sheet's code, and therefore cannot contain
    --   whitespace.
    --   
    --   By default, this is simply implemented via the 'Show' class.
    default fieldName :: (Show f) => f -> T.Text
    fieldName = T.pack . show

    fieldType :: f -> FieldType
    -- ^ The type of the field.

    fieldDefault :: f -> Maybe T.Text
    -- ^ The default value of the field. By default, this is @'Just' "0"@ for
    --   'NumberF's and 'Nothing' for all other fields.
    fieldDefault field
        | NumberF <- fieldType field = Just "0"
        | otherwise                  = Nothing

    fieldWorker :: f -> Maybe JStat
    -- ^ The sheet worker associated with the field. By default, this is
    --   'Nothing' for all fields.
    fieldWorker _ = Nothing

-- | A type with an instance of this class represents a set of fields @r@ that
--   exist within a repeating section of the Roll20 sheet.
--
--   Why is this different from 'SheetSection' and not just a subclass?
--
--   Because each repeating section is its own namespace, so we have to be
--   careful to treat them appropriately. It is incorrect to mix fields from
--   multiple repeating sections together, or to reference them outside of the
--   appropriate repeating section.
class (KnownSymbol (RepeatingSectionName r)) => RepeatingSection r where
    type RepeatingSectionName r :: Symbol

    repeatingFieldName :: r -> T.Text
    -- ^ The (internal) name for a given field. This is the name that will be
    --   used inside of the sheet's code, and therefore cannot contain
    --   whitespace.
    --   
    --   By default, this is simply implemented via the 'Show' class.
    default repeatingFieldName :: (Show r) => r -> T.Text
    repeatingFieldName = T.pack . show

    repeatingFieldType :: r -> FieldType
    -- ^ The type of the field.

    repeatingFieldDefault :: r -> Maybe T.Text
    -- ^ The default value of the field. By default, this is @'Just' "0"@ for
    --   'NumberF's and 'Nothing' for all other fields.
    repeatingFieldDefault field
        | NumberF <- repeatingFieldType field = Just "0"
        | otherwise                           = Nothing

    repeatingFieldWorker :: r -> Maybe JStat
    -- ^ The sheet worker associated with the field. By default, this is
    --   'Nothing' for all fields.
    repeatingFieldWorker _ = Nothing

-- | @repeatingSectionName \@r@ returns the associated 'RepeatingSectionName'
--   for a repeating section.
repeatingSectionName :: forall r. (RepeatingSection r) => T.Text
repeatingSectionName = T.pack . symbolVal $ Proxy @(RepeatingSectionName r)

-- | @repeatingSectionName' repeatingField@ returns the associated
--   'RepeatingSectionName' for a particular field in a repeating section.
repeatingSectionName' :: (RepeatingSection r) => r -> T.Text
repeatingSectionName' (_ :: r) = repeatingSectionName @r
