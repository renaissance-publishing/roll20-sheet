{-# OPTIONS_ghc -Wno-orphans #-}
module Roll20.SheetWorker 
    (
    -- * Predefined workers for auto-calculating values
    -- $autocalc
      autocalculate, autocalculateRepeating
    , floor'

    -- * Lower-level Sheet Worker API wrappers
    , IsLambda ( toLambda )
    -- ** @onEvent@
    , SheetEvent(..)
    , onEvent, withSectionId

    -- ** @RepeatingSectionId@
    , RepeatingSectionId
    , withEachSectionId, withNewSectionId, removeSectionById

    -- ** @getAttrs@
    , getAttrs, getAttrs'
    , getValueAtField, getValueAtRepeatingField

    -- ** @setAttrs@
    , setAttrs, setAttrs'
    , setAttr, setAttr'

    -- * Reexports
    , JExpr, JStat
    ) where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Ratio
import qualified Data.Text as T
import Language.Javascript.JMacro

import GHC.TypeNats ( KnownNat, type (+) )

import Roll20.FieldList
import Roll20.Field.Types

--

data SheetEvent where
    -- | A @FieldChanged@ event occurs when the field @f@'s value changes.
    --
    --   __Note:__ the event only occurs if the actual value is changed, not
    --   merely when the underlying HTML fields are edited.
    FieldChanged :: SheetSection f => f -> SheetEvent
    -- | A @RepeatingFieldChanged@ event occurs when the repeating field @r@'s
    --   value changes.
    --
    --   __Note:__ the event only occurs if the actual value is changed, not
    --   merely when the underlying HTML fields are edited.
    RepeatingFieldChanged :: RepeatingSection r => r -> SheetEvent
    -- | A @Clicked@ event occurs when the action button with the corresponding
    --   name is clicked.
    --
    --   This name should /not/ include the prefix @act_@.
    Clicked :: T.Text -> SheetEvent

toEventName :: SheetEvent -> T.Text
toEventName (FieldChanged f) = "change:" <> fieldName f
toEventName (RepeatingFieldChanged rf) = T.concat event
    where
        event = ["change:repeating_", sectionName, ":", repeatingFieldName rf]
        sectionName = repeatingSectionName' rf
toEventName (Clicked c) = "clicked:" <> c

instance ToJExpr SheetEvent where
    toJExpr = toJExpr . toEventName
    toJExprFromList = toJExpr . T.intercalate " " . fmap toEventName

--

-- | @onEvent evs fn@ registers the unary event handler @fn@ to run when any
--   event in @evs@ occurs.
onEvent :: (IsLambda 1 fn) => [SheetEvent] -> fn -> JStat
onEvent es handler = [jmacro| on `(es)` `(handler')` |]
    where
        handler' = toLambda @1 handler

-- | A @RepeatingSectionId@ specifies an individual row within the scope of a
--   repeating section.
newtype RepeatingSectionId = RepeatingSectionId { getRSId :: JExpr }
                           deriving (Show, Eq)

{-| 
@withSectionId@ extracts the 'RepeatingSectionId' from the event object passed
to an 'onEvent' handler and supplies it to the function that it is given.

Example:

@
onEvent [RepeatingFieldChanged ExampleField] $ withSectionId $ \\sid -> -- ...
@
-}
withSectionId :: (RepeatingSectionId -> a) -> (JExpr -> a)
withSectionId fn e = fn (RepeatingSectionId sid)
    where
        triggerName = SelExpr e (StrI "triggerName")
        firstIndex = [jmacroE| `(triggerName)`.lastIndexOf '_' `(lastIndex - 1)` |]
        lastIndex = [jmacroE| `(triggerName)`.lastIndexOf '_' |]
        sid = [jmacroE| `(triggerName)`.substring `(firstIndex + 1)` `(lastIndex)` |]

rfNameWithSectionId :: forall r. (RepeatingSection r) => RepeatingSectionId -> r -> JExpr
rfNameWithSectionId (RepeatingSectionId sid) r = [jmacroE| `(idPrefix)`.concat `(sid)` "_" `(repeatingFieldName r)` |]
    where
        idPrefix = T.concat ["repeating_", sectionName, "_"]
        sectionName = repeatingSectionName @r

-- | @withNewSectionId@ provides a fresh 'RepeatingSectionId' that can be used
--   to create a new row in a repeating section.
withNewSectionId :: (RepeatingSectionId -> JStat) -> JStat
withNewSectionId fn = [jmacro| `(fn')` generateRowID() |]
    where
        fn' = toLambda $ fn . RepeatingSectionId

-- | @withEachSectionId \@r@ iterates over each row in a repeating section @r@.
withEachSectionId :: forall r. (RepeatingSection r) => (RepeatingSectionId -> JStat) -> JStat
withEachSectionId fn = [jmacro| getSectionIDs `(sectionName)` (\ids -> ids.forEach `(fn')`) |]
    where
        sectionName = repeatingSectionName @r
        fn' = toLambda $ fn . RepeatingSectionId

-- | @removeSectionById \@r sid@ removes a row with 'RepeatingSectionId' @sid@
--   from the repeating section @r@.
removeSectionById :: forall r. (RepeatingSection r) => RepeatingSectionId -> JStat
removeSectionById (RepeatingSectionId sid) = [jmacro| removeRepeatingRow (`(idPrefix)`.concat `(sid)`) |]
    where
        idPrefix = T.concat ["repeating_", sectionName, "_"]
        sectionName = repeatingSectionName @r

--

rawGetAttrs :: (IsLambda 1 fn) => [JExpr] -> fn -> JStat
rawGetAttrs names handler = [jmacro| getAttrs `(names)` `(handler')`|]
    where
        handler' = toLambda @1 handler

-- | @getAttrs fs fn@ gets the values of each globally-scoped sheet field in
--   @fs@ and provides the result object to @fn@.
getAttrs :: (SheetSection f, IsLambda 1 fn) => [f] -> fn -> JStat
getAttrs fs = rawGetAttrs fieldNames
    where
        fieldNames = toJExpr . fieldName <$> fs

-- | @getAttrs' fs rfs sid fn@ gets the values of each globally-scoped sheet
--   field in @fs@ and each repeating section field in @rfs@ (based on the row
--   ID in @sid@) and passes the result object to @fn@.
--
--   Calls to @getAttrs@ and @getAttrs'@ cannot be nested, and thus this
--   function takes both scoped and global parameters.
getAttrs' :: (SheetSection f, RepeatingSection r, IsLambda 1 fn) => [f] -> [r] -> RepeatingSectionId -> fn -> JStat
getAttrs' fs rfs sid = rawGetAttrs $ fNames ++ rfNames
    where
        fNames = toJExpr . fieldName <$> fs
        rfNames = rfNameWithSectionId sid <$> rfs

--

rawSetAttrs :: [(JExpr, JExpr)] -> JStat
rawSetAttrs kvs =  [jmacro| 
    var as = {};
    `(kvs)`.forEach \kv {
        as[kv[0]] = kv[1];
    };
    setAttrs as;
    |]

-- | Given a list of globally scoped field-value pairs, @setAttrs@ sets the
--   values of the underlying Roll20 fields.
setAttrs :: (SheetSection f) => [(f, JExpr)] -> JStat
setAttrs kvs = rawSetAttrs kvs'
    where
        kvs' = first (toJExpr . fieldName) <$> kvs

-- | Given a list of repeating section field-value pairs, @setAttrs'@ sets the
--   values of the underlying Roll20 fields.
setAttrs' :: (RepeatingSection r) => [(r, JExpr)] -> RepeatingSectionId -> JStat
setAttrs' kvs sid = rawSetAttrs kvs'
    where
        kvs' = first (rfNameWithSectionId sid) <$> kvs

-- | @setAttr@ sets the value of a single globally scoped Roll20 field.
setAttr :: (SheetSection f) => f -> JExpr -> JStat
setAttr k v = setAttrs [(k, v)]

-- | @setAttr'@ sets the value of a single repeating section Roll20 field.
setAttr' :: (RepeatingSection r) => r -> JExpr -> RepeatingSectionId -> JStat
setAttr' k v = setAttrs' [(k, v)]

--

instance Fractional JExpr where
    fromRational rat = n / d
        where
            n = fromInteger (numerator rat)
            d = fromInteger (denominator rat)
    (/) = InfixExpr "/"

-- | This is provided because it is not possible to provide an 'RealFrac'
--   instance, as Haskell somewhat sensibly insists that it be possible to
--   round a 'RealFrac' into a member of any 'Integral' type, but it is
--   often necessary when writing auto-calculation rules.
floor' :: JExpr -> JExpr
floor' x = ApplExpr (jsv "Math.floor") [x]

rawGetValueAtField :: JExpr -> FieldType -> JExpr -> JExpr
rawGetValueAtField f ty vs
    | NumberF <- ty = InfixExpr "|" v 0
    | otherwise     = v
    where
        v = IdxExpr vs f

-- | @getValueAtField f v@ extracts the value of field @f@ from a 'getAttrs'
--   or 'getAttrs'' result object @v@.
getValueAtField :: (SheetSection f) => f -> JExpr -> JExpr
getValueAtField f = rawGetValueAtField (toJExpr $ fieldName f) (fieldType f)

-- | @getValueAtRepeatingField r sid v@ extracts the value of repeating section
--   field @r@ in row @sid@ from a 'getAttrs'' result object @v@.
getValueAtRepeatingField :: (RepeatingSection r) 
                         => r -> RepeatingSectionId -> JExpr -> JExpr
getValueAtRepeatingField r sid = rawGetValueAtField name ty
    where
        name = rfNameWithSectionId sid r
        ty = repeatingFieldType r

--

-- | The @IsLambda@ class facilitates converting functions of the form
--   @'JExpr' -> 'JExpr' -> ... -> a@ into Javascript lambda expressions,
--   where @n@ is the number of arguments and @a@ is either 'JExpr' or 'JStat'.
class (KnownNat n) => IsLambda n a | a -> n where
    toLambda :: a -> JExpr
    toLambda f = ValExpr . UnsatVal . IS $ do
        (stat, ns) <- runIdentSupply $ toLambda' f []
        return $ JFunc ns stat

    toLambda' :: a -> [Ident] -> IdentSupply (JStat, [Ident])

instance IsLambda 0 JStat where
    toLambda' s ns = IS $ return (s, reverse ns)

instance IsLambda 0 JExpr where
    toLambda' e ns = IS $ return (ReturnStat e, reverse ns)

instance (a ~ JExpr, n ~ (m + 1), IsLambda m b, KnownNat n) => IsLambda n (a -> b) where
    toLambda' f ns = IS $ do
        n <- takeOne
        runIdentSupply $ toLambda' (f (ValExpr $ JVar n)) (n:ns)

takeOne :: State [Ident] Ident
takeOne =
    get >>= \case
        (x:xs) -> do
            put xs
            return x
        _ -> error "not enough elements"

--

-- $autocalc
-- Both of the following functions have intimidating types, but are ultimately
-- quite simple.

-- | @autocalculate f fs fn@ gets the values of the @n@ fields in @fs@, and
--   passes their values to the @n@-argument function @fn@, setting the value
--   of @f@ to its result.
autocalculate :: forall f n fs fn . (SheetSection f, IsLambda n fn, FieldList fs f n) => f -> fs -> fn -> JStat
autocalculate dst src fn =
    onEvent (FieldChanged <$> src') $ \_ ->
        getAttrs src' $ \v ->
            setAttr dst (ApplExpr fn' $ values v)
    where
        src' = fieldList @_ @f @n src
        fn' = toLambda fn
        values v = getValueAtField <$> src' <*> pure v

-- |  @autocalculateRepeating r rs fs fn@ gets the value of the @a@ repeating
--    section fields in @rs@ and the @b@ globally-scoped fields in @fs@, and
--    passes their values to the @(a + b)@-argument function @fn@, setting
--    the value of @r@ (in the row where the change occurred if triggered by
--    a change to @rs@, or to all rows if triggered by a change to @fs@)
--    to the result.
autocalculateRepeating :: forall r f a b c rs fs fn .
                          ( RepeatingSection r, SheetSection f, IsLambda c fn
                          , FieldList rs r a, FieldList fs f b, (a + b) ~ c
                          )
                       => r -> rs -> fs -> fn -> JStat
autocalculateRepeating dst rsrc src fn = rchanged <> changed
    where
        rchanged = onEvent revents $       withSectionId        $ doUpdate
        changed  = onEvent events  $ \_ -> withEachSectionId @r $ doUpdate

        doUpdate sid =
            getAttrs' src' rsrc' sid $ \v ->
                setAttr' dst (ApplExpr fn' $ allValues sid v) sid

        rsrc' = fieldList @_ @r @a rsrc
        src'  = fieldList @_ @f @b src

        revents = fmap RepeatingFieldChanged rsrc'
        events = fmap FieldChanged src'

        fn' = toLambda fn
        values v = fmap (\f -> getValueAtField f v) src'
        rvalues sid v = fmap (\r -> getValueAtRepeatingField r sid v) rsrc'
        allValues sid = rvalues sid <> values
