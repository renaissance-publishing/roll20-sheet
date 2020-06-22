{-# OPTIONS_ghc -Wno-partial-type-signatures #-}
module Roll20.RollTemplate
    ( Roll( Die, Comment, Field, Query, EnumQuery, ToTracker
          , Min, Max
          , RawText
          )
    , repFieldRoll
    , d4, d6, d8, d10, d12, d100, modifier
    , renderRoll
    , IsRollTemplate(..)
    , RollWithTemplate(), rollWithTemplate
    , Arg(), Optional(..), IsNamedParameter(..), constArg
    , arg, argExists, argDoesNotExist
    , argIsCrit, argIsNotCrit
    , argIsFumble, argIsNotFumble
    , argEqual, argNotEqual
    , argLessThan, argLessThanOrEqual
    , argGreaterThan, argGreaterThanOrEqual
    , argBetween, argNotBetween
    , module Data.Functor.Trans.Tagged
    ) where

import Blaze.ByteString.Builder ( Builder )
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Data.List ( intersperse )
import Data.Functor.Trans.Tagged
import Data.Proxy ( Proxy(..) )
import Data.String ( IsString(..) )
import qualified Data.Text as T
import GHC.TypeLits

import Lucid.Base
import Lucid.Html5 ( class_ )

import Roll20.Field.Types
import Roll20.Types

--

data Roll where
    Die       :: Roll -> Roll -> Roll
    Comment   :: Roll -> T.Text -> Roll
    Field     :: (SheetSection f) => f -> Roll
    RepField  :: (RepeatingSection r name) => r -> Roll
    Query     :: T.Text -> Maybe Integer -> Roll
    EnumQuery :: T.Text -> [(T.Text, Roll)] -> Roll
    ToTracker :: Roll -> Roll
    Min       :: [Roll] -> Roll
    Max       :: [Roll] -> Roll

    --

    Template :: T.Text -> [(T.Text, Roll)] -> Roll

    Plus :: Roll -> Roll -> Roll
    Minus :: Roll -> Roll -> Roll
    Times :: Roll -> Roll -> Roll
    Abs :: Roll -> Roll
    Signum :: Roll -> Roll
    ConstInt :: Integer -> Roll
    RawText :: T.Text -> Roll

d4, d6, d8, d10, d12, d100 :: Roll -> Roll
d4   = flip Die 4
d6   = flip Die 6
d8   = flip Die 8
d10  = flip Die 10
d12  = flip Die 12
d100 = flip Die 100

modifier :: Roll
modifier = Comment (Query "Modifier" $ Just 0) "Modifier"

repFieldRoll :: (RepeatingSection r name, Monad m) => r -> ASheet (TaggedT r m) Roll
repFieldRoll = return . RepField

instance Show Roll where
    show = T.unpack . renderRoll

instance Num Roll where
    (+) = Plus
    (-) = Minus
    (*) = Times
    abs = Abs
    signum = Signum
    fromInteger = ConstInt

instance IsString Roll where
    fromString = RawText . T.pack

renderRollGroup :: [Roll] -> T.Text
renderRollGroup = wrap "{" "}" . T.intercalate ", " . fmap renderRoll

renderRoll :: Roll -> T.Text
renderRoll (Die n sides) = T.concat [renderRoll n, "d", renderRoll sides]
renderRoll (Comment r c) = T.concat [wrapParens r, "[", c, "]"]
renderRoll (Field f) = wrap "@{" "}" $ fieldName f
renderRoll (RepField r) = wrap "@{" "}" $ repeatingFieldName r
renderRoll (Query n d) = T.concat ["?{", n, foldMap go d, "}"]
    where go def = "|" <> T.pack (show def)
renderRoll (EnumQuery n o) = T.concat ["?{", n, foldMap go o, "}"]
    where go (n', v) = T.concat ["|", n', ",", T.pack (show v)]
renderRoll (ToTracker r) = renderRoll r <> " &{tracker}"
renderRoll (Min rs) = renderRollGroup rs <> "kl1"
renderRoll (Max rs) = renderRollGroup rs <> "kh1"

renderRoll (Template name params) = templateSyn <> foldMap go params
    where
        templateSyn = wrap "&{template:" "}" name

        go (pname, param) = T.concat [" {{", pname, "=", wrapInlineRoll param, "}}"]

renderRoll (Plus l r)   = T.concat [wrapParens l, " + ", wrapParens r]
renderRoll (Minus l r)  = T.concat [wrapParens l, " - ", wrapParens r]
renderRoll (Times l r)  = T.concat [wrapParens l, " * ", wrapParens r]
renderRoll (Abs a)      = T.concat [wrap "abs(" ")" $ renderRoll a]
renderRoll (Signum sig) = T.concat [wrapParens sig, " / ", renderRoll $ Abs sig]
renderRoll (ConstInt i) = T.pack (show i)
renderRoll (RawText t)  = t

wrapInlineRoll :: Roll -> T.Text
wrapInlineRoll roll
    | shouldWrap = wrap "[[" "]]" $ renderRoll roll
    | otherwise  = renderRoll roll
    where
        shouldWrap = case roll of
            Field _    -> False
            RepField _ -> False
            RawText _  -> False
            _ -> True

wrapParens :: Roll -> T.Text
wrapParens roll
    | shouldWrap = wrap "(" ")" $ renderRoll roll
    | otherwise  = renderRoll roll
    where
        shouldWrap = case roll of
            Plus _ _ -> True
            Minus _ _ -> True
            Times _ _ -> True
            Signum _ -> True
            _ -> False

wrap :: T.Text -> T.Text -> T.Text -> T.Text
wrap start end middle = T.concat [start, middle, end]

--

class IsRollTemplate a where
    rollTemplate :: KnownSymbol name => Tagged name a -> RollTemplate ()

instance IsRollTemplate (RollTemplate ()) where
    rollTemplate (t :: Tagged name _) = rolltemplate_ (symbolValT @name) t'
        where
            t' = untag t

instance (Arg n ~ a, IsNamedParameter n, IsRollTemplate b) => IsRollTemplate (a -> b) where
    rollTemplate fn = rollTemplate @b (fmap ($ Named @n) fn)

--

rollWithTemplate :: (KnownSymbol name, RollWithTemplate t r) => Tagged name t -> r
rollWithTemplate t = rollWithTemplate' t []

class (IsRollTemplate t) => RollWithTemplate t r | t -> r where
    rollWithTemplate' :: (KnownSymbol n) => Tagged n t -> [(T.Text, Roll)] -> r

instance RollWithTemplate (RollTemplate ()) Roll where
    rollWithTemplate' (_ :: Tagged name _) rs = Template templateName (reverse rs)
        where
            templateName = symbolValT @name

instance (RollWithTemplate t r, KnownSymbol n) => RollWithTemplate (Arg (n :: Symbol) -> t) (Roll -> r) where
    rollWithTemplate' fn rs = \r -> rollWithTemplate' fn' ((n', r):rs)
        where
            n' = symbolValT @n
            fn' = fmap ($ Named @n) fn

instance (RollWithTemplate t r, KnownSymbol n) => RollWithTemplate (Arg ('Optional (n :: Symbol)) -> t) (Maybe Roll -> r) where
    rollWithTemplate' fn rs = unwrapOptional
        where
            unwrapOptional (Just r) = rollWithTemplate' fn' ((n', r):rs)
            unwrapOptional Nothing  = rollWithTemplate' fn' rs

            n' = symbolValT @n
            fn' = fmap ($ Named @('Optional n)) fn

--

data Optional param = Optional param

class IsNamedParameter (param :: k) where
    parameterName :: String

instance (KnownSymbol name) => IsNamedParameter name where
    parameterName = symbolVal $ Proxy @name

instance (IsNamedParameter param) => IsNamedParameter ('Optional param) where
    parameterName = parameterName @_ @param

--

data Arg (a :: k) where
    Named :: IsNamedParameter param => Arg param
    Constant :: Int -> Arg Int

constArg :: Int -> Arg Int
constArg = Constant

--

arg :: (IsNamedParameter name) => Arg name -> RollTemplate ()
arg a = HtmlT $ return (\_ -> s "{{" <> renderArg a <> s "}}", ())

argExists :: (IsNamedParameter name) => Arg name -> RollTemplate a -> RollTemplate a
argExists = makeProperty (s "#") (s "/") . renderArg

argDoesNotExist :: (IsNamedParameter name) => Arg name -> RollTemplate a -> RollTemplate a
argDoesNotExist = makeProperty (s "^") (s "/") . renderArg

--

argIsCrit, argIsNotCrit :: (IsNamedParameter name) => Arg name
                        -> RollTemplate a -> RollTemplate a
(argIsCrit, argIsNotCrit) = functionTag (unaryFunction "rollWasCrit()")

argIsFumble, argIsNotFumble :: (IsNamedParameter name) => Arg name
                            -> RollTemplate a -> RollTemplate a
(argIsFumble, argIsNotFumble) = functionTag (unaryFunction "rollWasFumble()")

argEqual, argNotEqual :: (IsNamedParameter name) => Arg name -> Arg (arg :: k)
                      -> RollTemplate a -> RollTemplate a
(argEqual, argNotEqual) = functionTag (binaryFunction "rollTotal()") 

argGreaterThan, argLessThanOrEqual :: (IsNamedParameter name) => Arg name -> Arg (arg :: k)
                                   -> RollTemplate a -> RollTemplate a
(argGreaterThan, argLessThanOrEqual) = functionTag (binaryFunction "rollGreater()") 

argLessThan, argGreaterThanOrEqual :: (IsNamedParameter name) => Arg name -> Arg (arg :: k)
                                   -> RollTemplate a -> RollTemplate a
(argLessThan, argGreaterThanOrEqual) = functionTag (binaryFunction "rollLess()") 

argBetween, argNotBetween :: (IsNamedParameter name) => Arg name -> Arg (arg1 :: k1) -> Arg (arg2 :: k2)
                          -> RollTemplate a -> RollTemplate a
(argBetween, argNotBetween) = functionTag (ternaryFunction "rollBetween()")

--

functionTag :: (Builder -> Builder -> a) -> (a, a)
functionTag fn = (fn (s "#") (s "/"), fn (s "#^") (s "/^"))

unaryFunction :: (Functor m, IsNamedParameter name) => String
              -> Builder -> Builder
              -> Arg name
              -> HtmlT m a -> HtmlT m a
unaryFunction fnName start end x = makeProperty start end (spaceSep fn)
    where fn = [s fnName, renderArg x]

binaryFunction :: (Functor m, IsNamedParameter name) => String
               -> Builder -> Builder
               -> Arg name -> Arg (arg :: k)
               -> HtmlT m a -> HtmlT m a
binaryFunction fnName start end x y = makeProperty start end (spaceSep fn)
    where fn = [s fnName, renderArg x, renderArg y]

ternaryFunction :: (Functor m, IsNamedParameter name) => String
                -> Builder -> Builder
                -> Arg name -> Arg (arg1 :: k1) -> Arg (arg2 :: k2)
                -> HtmlT m a -> HtmlT m a
ternaryFunction fnName start end x y z = makeProperty start end (spaceSep fn)
    where fn = [s fnName, renderArg x, renderArg y, renderArg z]

makeProperty :: (Functor m) => Builder -> Builder -> Builder -> HtmlT m a -> HtmlT m a
makeProperty start end prop child = HtmlT $ go <$> runHtmlT child
    where
        go ~(childRaw, a) =
            (\_ ->
                s "{{" <> start <> prop <> s "}}"
             <> childRaw mempty
             <> s "{{" <> end <> prop <> s "}}"
            , a)

renderArg :: forall a. Arg a -> Builder
renderArg Named = s (parameterName @_ @a)
renderArg (Constant n) = Blaze.fromShow n

spaceSep :: [Builder] -> Builder
spaceSep = mconcat . intersperse (Blaze.fromChar ' ')

s :: String -> Builder
s = Blaze.fromString

rolltemplate_ :: Term arg result => T.Text -> arg -> result
rolltemplate_ name = termWith "rolltemplate" [class_ className]
    where className = "sheet-rolltemplate-" <> name

symbolValT :: forall t. KnownSymbol t => T.Text
symbolValT = T.pack (symbolVal $ Proxy @t)
