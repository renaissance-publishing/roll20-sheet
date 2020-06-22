module Roll20.Tabbed.Types
    ( TabbedSheet(..)
    , allTabs
    , TabbedSheetSection(..)
    ) where

import qualified Data.Text as T
import Data.Universe.Class ( Finite(..) )
import Roll20.Field.Types
import Roll20.Types

class (Finite a) => TabbedSheet a where
    tabName :: a -> T.Text
    tabSlug :: a -> T.Text
    tabBody :: a -> SheetR ()
    sheetGlobal :: Sheet ()

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
