module Roll20.Field
    (
    -- * Defining fields
      FieldType(..)
    -- ** Global fields
    , SheetSection(..)
    -- ** Scoped fields
    , RepeatingSection(..)
    , repeatingSectionName, repeatingSectionName'
    -- * Displaying fields
    -- ** Global fields
    , field, field', hiddenField
    -- ** Scoped fields
    , repeating
    , repField, repField', hiddenRepField
    -- ** Buttons
    , actionButton, rollButton
    -- * Reexports
    , module Roll20.Types
    ) where

import Data.Functor.Trans.Tagged
import qualified Data.Text as T

import Roll20.Field.Types
import Roll20.RollTemplate ( Roll, renderRoll )
import Roll20.Types

import Lucid

--

mkSelectOpt :: (Monad m) => T.Text -> HtmlT m ()
mkSelectOpt opt = option_ [value_ opt] $ toHtml opt

rawField :: (Monad m) => T.Text -> FieldType -> Maybe T.Text -> HtmlT m ()
rawField name type' def =
    case type' of
        TextareaF -> 
            let
                attrs = nameAttr : fieldTypeAttrs TextareaF
                def' = maybe "" toHtml def
            in textarea_ attrs def'
        SelectF opts ->
            let 
                opts' = mapM_ mkSelectOpt opts
            in select_ [nameAttr] opts'
        _ ->
            let
                attrs = case defAttr of
                    Just defAttr' -> nameAttr : defAttr' : fieldTypeAttrs type'
                    Nothing       -> nameAttr : fieldTypeAttrs type'
                defAttr = value_ <$> def
            in input_ attrs
    where
        nameAttr = name_ $ "attr_" <> name

field :: (SheetSection f) => f -> Sheet ()
field f = rawField (fieldName f) (fieldType f) (fieldDefault f)

hiddenField :: (SheetSection f) => f -> Sheet ()
hiddenField f = rawField (fieldName f) HiddenF Nothing

field' :: (SheetSection f) => f -> SheetR ()
field' f = isEditable >>= \case
    ReadWrite -> field f
    ReadOnly  -> do
        case fieldType f of
            TextareaF -> span_ [name_ $ "attr_" <> fieldName f, class_ "sheet-ro-textarea"] ""
            SelectF _ -> span_ [name_ $ "attr_" <> fieldName f, class_ "sheet-ro-select"] ""
            _ -> field f `with` [readonly_ "true"]
        maybe (return ()) tellJs (fieldWorker f)

repField :: (RepeatingSection r) => r -> ASheet (TaggedT r m) ()
repField f = rawField (repeatingFieldName f)
                      (repeatingFieldType f)
                      (repeatingFieldDefault f)

hiddenRepField :: (RepeatingSection r) => r -> ASheet (TaggedT r m) ()
hiddenRepField f = rawField (repeatingFieldName f)
                            HiddenF
                            Nothing

repField' :: (RepeatingSection r) => r -> ASheetR (TaggedT r m) ()
repField' f = isEditable >>= \case
    ReadWrite -> repField f
    ReadOnly  -> do
        case repeatingFieldType f of
            TextareaF -> span_ [name_ $ "attr_" <> repeatingFieldName f, class_ "sheet-ro-textarea"] ""
            SelectF _ -> span_ [name_ $ "attr_" <> repeatingFieldName f, class_ "sheet-ro-select"] ""
            _ -> repField f `with` [readonly_ "true"]
        maybe (return ()) tellJs (repeatingFieldWorker f)

--

actionButton :: (Applicative m) => T.Text -> HtmlT m () -> HtmlT m ()
actionButton name = button_ [type_ "action", name_ rawName]
    where
        rawName = "act_" <> name

rollButton :: (Applicative m) => Roll -> HtmlT m () -> HtmlT m ()
rollButton roll = button_ [type_ "roll", value_ val]
    where
        val = renderRoll roll

--

repeating :: forall r m. (Monad m, RepeatingSection r) => ASheet (TaggedT r m) () -> ASheet m ()
repeating = fieldset_ [class_ repTag] . withSectionTag
    where
        repTag = "repeating_" <> repeatingSectionName @r
