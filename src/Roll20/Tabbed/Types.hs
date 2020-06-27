module Roll20.Tabbed.Types
    ( TabbedSheet(..)
    , allTabs
    , TabbedSheetSection(..)
    ) where

import qualified Data.Text as T
import Data.Universe.Class ( Finite(..) )
import Roll20.Field.Types
import Roll20.Types

-- | A type with an instance of @TabbedSheet@ represents the set of tabs in a
--   particular sheet.
class (Finite a) => TabbedSheet a where
    tabName :: a -> T.Text
    -- ^ The human readable name for a tab.
    tabSlug :: a -> T.Text
    -- ^ A name, valid to include in a CSS class, for a tab.
    tabBody :: a -> SheetR ()
    -- ^ The contents of the tab, parameterized by the current readability of
    --   the tab.
    sheetGlobal :: Sheet ()
    -- ^ Global sheet layout that is not provided directly by this library.
    --
    --   Used for common sections that should appear regardless of the
    --   currently selected tab.

allTabs :: (TabbedSheet a) => [a]
allTabs = universeF

data TabbedSheetSection = TabState | Editable
                      deriving (Show, Eq)

instance SheetSection TabbedSheetSection where
    fieldName = T.pack . show
    
    fieldType _ = HiddenF
    
    fieldDefault TabState = Just "default"
    fieldDefault Editable = Just "false"
    
    fieldWorker _ = Nothing
