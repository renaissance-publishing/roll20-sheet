module Roll20.FieldList.Class ( FieldList(..) ) where

import GHC.TypeNats ( KnownNat )

class (KnownNat n) => FieldList fs f n | f n -> fs, fs f -> n, fs n -> f
    where fieldList :: fs -> [f]

instance FieldList f f 1 where
    fieldList = pure
