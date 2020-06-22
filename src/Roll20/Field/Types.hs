module Roll20.Field.Types
    ( FieldType(..), fieldTypeAttrs
    , SheetMode(..)
    , SheetSection(..)
    , RepeatingSection(..)
    , repeatingSectionName, repeatingSectionName'
    ) where

import Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Language.Javascript.JMacro ( JStat )
import qualified Lucid as L

import Roll20.Types

data FieldType = HiddenF
               | TextF
               | NumberF
               | CheckboxF
               | TextareaF
               | SelectF [T.Text]
               deriving (Eq, Show)

fieldTypeAttrs :: FieldType -> [L.Attribute]
fieldTypeAttrs HiddenF     = [L.type_ "hidden"]
fieldTypeAttrs TextF       = [L.type_ "text", L.spellcheck_ "false"]
fieldTypeAttrs NumberF     = [L.type_ "number"]
fieldTypeAttrs CheckboxF   = [L.type_ "checkbox"]
fieldTypeAttrs TextareaF   = [L.spellcheck_ "false"]
fieldTypeAttrs (SelectF _) = []

class SheetSection f where
    fieldName :: f -> T.Text
    default fieldName :: (Show f) => f -> T.Text
    fieldName = T.pack . show

    fieldType :: f -> FieldType
    
    fieldDefault :: f -> Maybe T.Text
    fieldDefault field
        | NumberF <- fieldType field = Just "0"
        | otherwise                  = Nothing

    fieldWorker :: f -> Maybe JStat
    fieldWorker _ = Nothing

-- Why is this different and not just a subclass?
--
-- Because each repeating section is its own namespace,
-- so we have to be careful to treat them appropriately.
class (KnownSymbol name) => RepeatingSection r name | r -> name where
    repeatingFieldName :: r -> T.Text
    default repeatingFieldName :: (Show r) => r -> T.Text
    repeatingFieldName = T.pack . show

    repeatingFieldType :: r -> FieldType

    repeatingFieldDefault :: r -> Maybe T.Text
    repeatingFieldDefault field
        | NumberF <- repeatingFieldType field = Just "0"
        | otherwise                           = Nothing

    repeatingFieldWorker :: r -> Maybe JStat
    repeatingFieldWorker _ = Nothing

repeatingSectionName :: forall f name. (RepeatingSection f name) => T.Text
repeatingSectionName = T.pack $ symbolVal (Proxy @name)

repeatingSectionName' :: (RepeatingSection f name) => f -> T.Text
repeatingSectionName' (_ :: f) = repeatingSectionName @f
