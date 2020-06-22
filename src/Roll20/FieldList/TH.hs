module Roll20.FieldList.TH ( mkFieldListInstances ) where

import Control.Monad ( replicateM )
import Data.Foldable ( foldl' )
import Language.Haskell.TH
import Roll20.FieldList.Class

mkFieldListInstance :: Int -> Q [Dec]
mkFieldListInstance n = do
    f <- VarT <$> newName "f"
    let fs = foldl' AppT (TupleT n) (replicate n f)
        tn = litT . numTyLit . toInteger $ n
    xs <- replicateM n (newName "x")
    let pat = tupP . fmap varP $ xs
        body = listE . fmap varE $ xs
    [d| instance FieldList $(pure fs) $(pure f) $tn where fieldList $pat = $body |]

mkFieldListInstances :: Int -> Q [Dec]
mkFieldListInstances upto = do
    insts <- mapM mkFieldListInstance [2..upto]
    pure (concat insts)
