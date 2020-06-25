{-|
This module contains the types and typeclasses used to define both global and
correctly-namespaced repeating field sets, and the functions used to render
them into a "Lucid" template of the correct type.

Additionally, it includes helpers used to create action and roll buttons inside
of sheets, because their code is both simple and very similar to the other
functionality of this module.
-}
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

-- | Render a global field into a 'Sheet'.
field :: (SheetSection f) => f -> Sheet ()
field f = rawField (fieldName f) (fieldType f) (fieldDefault f)

-- | Render a hidden field that will mirror the value of the provided global
--   field into a 'Sheet'.
hiddenField :: (SheetSection f) => f -> Sheet ()
hiddenField f = rawField (fieldName f) HiddenF Nothing

-- | Render a global field into a sheet section that has variants for each
--   'SheetMode'.
--
--   Specifically, it renders the field in the same way as 'field' if the
--   current mode is 'ReadWrite', but if the current mode is 'ReadOnly':
--
--   * If the field is a 'TextareaF' or 'SelectF', the field is rendered as a
--     HTML @\<span>@ tag with the @"sheet-ro-textarea"@ or @"sheet-ro-select"@
--     classes, as appropriate.
--
--   * Otherwise, the field is rendered as the appropriate @\<input>@ tag as
--     normal, but with the @readonly@ attribute set.
field' :: (SheetSection f) => f -> SheetR ()
field' f = isEditable >>= \case
    ReadWrite -> field f
    ReadOnly  -> do
        case fieldType f of
            TextareaF -> span_ [name_ $ "attr_" <> fieldName f, class_ "sheet-ro-textarea"] ""
            SelectF _ -> span_ [name_ $ "attr_" <> fieldName f, class_ "sheet-ro-select"] ""
            _ -> field f `with` [readonly_ "true"]
        maybe (return ()) tellJs (fieldWorker f)

-- | Equivalent to 'field' for fields in a 'RepeatingSection', but the type
--   limits its use appropriately to ensure that it may only be used inside
--   the appropriate 'repeating' block.
repField :: (RepeatingSection r) => r -> ASheet (TaggedT r m) ()
repField f = rawField (repeatingFieldName f)
                      (repeatingFieldType f)
                      (repeatingFieldDefault f)

-- | Equivalent to 'hiddenField' for fields in a 'RepeatingSection', but the
--   type limits its use appropriately to ensure that it may only be used
--   inside the appropriate 'repeating' block.
hiddenRepField :: (RepeatingSection r) => r -> ASheet (TaggedT r m) ()
hiddenRepField f = rawField (repeatingFieldName f)
                            HiddenF
                            Nothing

-- | Equivalent to 'field'' for fields in a 'RepeatingSection', but the type
--   limits its use appropriately to ensure that it may only be used inside
--   the appropriate 'repeating' block.
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

-- | Renders an action button with the given name into the sheet. Note that
--   this function handles prepending @act_@, and therefore it should not
--   be used in the argument.
actionButton :: (Applicative m) => T.Text -> HtmlT m () -> HtmlT m ()
actionButton name = button_ [type_ "action", name_ rawName]
    where
        rawName = "act_" <> name

-- | Renders a roll button for the given roll into the sheet.
rollButton :: (Applicative m) => Roll -> HtmlT m () -> HtmlT m ()
rollButton roll = button_ [type_ "roll", value_ val]
    where
        val = renderRoll roll

--

-- | @repeating \@r@ introduces a repeating section containing fields of type
--   @r@ into the sheet. 
--
--   Generally, this should be used with an explicit type application to ensure
--   that the section being used is fixed to the correct value.
repeating :: forall r m. (Monad m, RepeatingSection r) => ASheet (TaggedT r m) () -> ASheet m ()
repeating = fieldset_ [class_ repTag] . withSectionTag
    where
        repTag = "repeating_" <> repeatingSectionName @r
