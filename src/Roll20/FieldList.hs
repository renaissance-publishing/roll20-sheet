{-# OPTIONS_ghc -Wno-orphans #-}
module Roll20.FieldList ( FieldList(..) ) where

import Roll20.FieldList.Class
import Roll20.FieldList.TH

$(mkFieldListInstances 16)
