{-# OPTIONS_ghc -Wno-orphans #-}
module Roll20.SheetWorker 
    ( floor', autocalculate, autocalculateRepeating
    , SheetEvent(..)
    , IsLambda(), JExpr, JStat
    , onEvent, withSectionId
    , withEachSectionId, withNewSectionId, removeSectionById
    , RepeatingSectionId
    , getAttrs, getAttrs'
    , getValueAtField, getValueAtRepeatingField
    , setAttrs, setAttrs'
    , setAttr, setAttr'
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
    FieldChanged :: SheetSection f => f -> SheetEvent
    RepeatingFieldChanged :: RepeatingSection r => r -> SheetEvent
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

onEvent :: (IsLambda fn 1) => [SheetEvent] -> fn -> JStat
onEvent es handler = [jmacro| on `(es)` `(handler')` |]
    where
        handler' = toLambda @1 handler

newtype RepeatingSectionId = RepeatingSectionId { getRSId :: JExpr }
                           deriving (Show, Eq)

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

withNewSectionId :: (RepeatingSectionId -> JStat) -> JStat
withNewSectionId fn = [jmacro| `(fn')` generateRowID() |]
    where
        fn' = toLambda $ fn . RepeatingSectionId

withEachSectionId :: forall r. (RepeatingSection r) => (RepeatingSectionId -> JStat) -> JStat
withEachSectionId fn = [jmacro| getSectionIDs `(sectionName)` (\ids -> ids.forEach `(fn')`) |]
    where
        sectionName = repeatingSectionName @r
        fn' = toLambda $ fn . RepeatingSectionId

removeSectionById :: forall r. (RepeatingSection r) => RepeatingSectionId -> JStat
removeSectionById (RepeatingSectionId sid) = [jmacro| removeRepeatingRow (`(idPrefix)`.concat `(sid)`) |]
    where
        idPrefix = T.concat ["repeating_", sectionName, "_"]
        sectionName = repeatingSectionName @r

--

rawGetAttrs :: (IsLambda fn 1) => [JExpr] -> fn -> JStat
rawGetAttrs names handler = [jmacro| getAttrs `(names)` `(handler')`|]
    where
        handler' = toLambda @1 handler

getAttrs :: (SheetSection f, IsLambda fn 1) => [f] -> fn -> JStat
getAttrs fs = rawGetAttrs fieldNames
    where
        fieldNames = toJExpr . fieldName <$> fs

getAttrs' :: (SheetSection f, RepeatingSection r, IsLambda fn 1) => [f] -> [r] -> RepeatingSectionId -> fn -> JStat
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

setAttrs :: (SheetSection f) => [(f, JExpr)] -> JStat
setAttrs kvs = rawSetAttrs kvs'
    where
        kvs' = first (toJExpr . fieldName) <$> kvs

setAttrs' :: (RepeatingSection r) => [(r, JExpr)] -> RepeatingSectionId -> JStat
setAttrs' kvs sid = rawSetAttrs kvs'
    where
        kvs' = first (rfNameWithSectionId sid) <$> kvs

setAttr :: (SheetSection f) => f -> JExpr -> JStat
setAttr k v = setAttrs [(k, v)]

setAttr' :: (RepeatingSection r) => r -> JExpr -> RepeatingSectionId -> JStat
setAttr' k v = setAttrs' [(k, v)]

--

instance Fractional JExpr where
    fromRational rat = n / d
        where
            n = fromInteger (numerator rat)
            d = fromInteger (denominator rat)
    (/) = InfixExpr "/"

floor' :: JExpr -> JExpr
floor' x = ApplExpr (jsv "Math.floor") [x]

rawGetValueAtField :: (JExpr, FieldType) -> JExpr -> JExpr
rawGetValueAtField (f, ty) vs
    | NumberF <- ty = InfixExpr "|" v 0
    | otherwise     = v
    where
        v = IdxExpr vs f

getValueAtField :: (SheetSection f) => f -> JExpr -> JExpr
getValueAtField f = rawGetValueAtField (toJExpr $ fieldName f, fieldType f)

getValueAtRepeatingField :: (RepeatingSection r) 
                         => r -> RepeatingSectionId -> JExpr -> JExpr
getValueAtRepeatingField r sid = rawGetValueAtField (name, ty)
    where
        name = rfNameWithSectionId sid r
        ty = repeatingFieldType r

--

toLambda :: forall n a. (IsLambda a n) => a -> JExpr
toLambda f = ValExpr . UnsatVal . IS $ do
    (stat, ns) <- runIdentSupply $ toLambda' f []
    return $ JFunc ns stat

class (KnownNat n) => IsLambda a n | a -> n where
    toLambda' :: a -> [Ident] -> IdentSupply (JStat, [Ident])

instance IsLambda JStat 0 where
    toLambda' s ns = IS $ return (s, reverse ns)

instance IsLambda JExpr 0 where
    toLambda' e ns = IS $ return (ReturnStat e, reverse ns)

instance (a ~ JExpr, n ~ (m + 1), IsLambda b m, KnownNat n) => IsLambda (a -> b) n where
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

autocalculate :: forall f n fs fn . (SheetSection f, IsLambda fn n, FieldList fs f n) => f -> fs -> fn -> JStat
autocalculate dst src fn =
    onEvent (FieldChanged <$> src') $ \_ ->
        getAttrs src' $ \v ->
            setAttr dst (ApplExpr fn' $ values v)
    where
        src' = fieldList @_ @f @n src
        fn' = toLambda fn
        values v = getValueAtField <$> src' <*> pure v

autocalculateRepeating :: forall r f a b c rs fs fn .
                          (RepeatingSection r, SheetSection f, IsLambda fn c
                          , FieldList rs r a, FieldList fs f b, (a + b) ~ c)
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
