module Roll20.Types 
    ( SheetMode(..), TemplateKind(..)
    , Sheet, SheetR, RollTemplate
    , ASheet, ASheetR
    , runTemplate
    , getRollTemplates
    , embedModalTemplate
    , tagsOnly
    , withSectionTag
    , tellRollTemplate, tellCss, tellJs
    , isEditable
    ) where

import Control.Monad.Morph ( hoist )
import Control.Monad.Reader ( ReaderT, MonadReader(..), runReaderT )
import Control.Monad.Writer.Strict ( Writer, MonadWriter(..), runWriter, censor )
import Data.Functor.Identity ( Identity )
import Data.Functor.Trans.Tagged ( TaggedT(..), mapTaggedT )

import Language.Javascript.JMacro ( JStat )
import Clay ( Css )
import Lucid.Base

--

data SheetMode = ReadOnly | ReadWrite
               deriving (Show, Eq)

--

data TemplateKind = SheetTemplate | RollTemplate
                  deriving (Show, Eq)

type Sheet a = forall m. ASheet m a
type SheetR a = forall m. ASheetR m a
type RollTemplate a = HtmlT (TaggedT 'RollTemplate Identity) a

type ASheet m a = (MonadWriter (JStat, Css, RollTemplate ()) m) => HtmlT (TaggedT 'SheetTemplate m) a
type ASheetR m a = (MonadReader SheetMode m) => ASheet m a

runTemplate :: (Monoid w) => HtmlT (TaggedT s (Writer w)) () -> (Html (), w)
runTemplate = runWriter . untagT . commuteHtmlT

getRollTemplates :: RollTemplate () -> Html ()
getRollTemplates = hoist untagT

embedModalTemplate :: (Monad m) => r -> HtmlT (TaggedT tag (ReaderT r m)) () -> HtmlT (TaggedT tag m) ()
embedModalTemplate r = hoist (mapTaggedT $ flip runReaderT r)

tagsOnly :: (MonadWriter w m, Monoid w) => HtmlT m () -> HtmlT m ()
tagsOnly = censor (const mempty)

withSectionTag :: (Monad m) => HtmlT (TaggedT tag1 (TaggedT tag2 m)) () -> HtmlT (TaggedT tag1 m) ()
withSectionTag = hoist (mapTaggedT untagT)

tellRollTemplate :: RollTemplate () -> Sheet ()
tellRollTemplate = tell . (mempty, mempty, )

tellCss :: Css -> Sheet ()
tellCss = tell . (mempty,, mempty)

tellJs :: JStat -> Sheet ()
tellJs = tell . (, mempty, mempty)

isEditable :: SheetR SheetMode
isEditable = ask
