module Roll20.CSS
    ( module Control.Monad
    , module Prelude

    , module Clay.Background
    , module Clay.Border
    , module Clay.Box
    , module Clay.Color
    , module Clay.Common
    , module Clay.Display
    , module Clay.Dynamic
    , module Clay.Flexbox
    , module Clay.Font
    , module Clay.Geometry
    , module Clay.Grid
    , module Clay.Pseudo
    , module Clay.Selector
    , module Clay.Size
    , module Clay.Stylesheet
    , module Clay.Text

    , module Roll20.CSS.Selectors
    , tshow
    ) where

import Control.Monad
import qualified Data.Text as T
import Prelude hiding ( all, not, round, rem, repeat )

import Clay.Background
import Clay.Border
import Clay.Box
import Clay.Color ( Color
                  , rgba, rgb, hsla, hsl, grayish, transparent
                  , setR, setG, setB, setA
                  , toRgba, toHsla
                  , (*.), (+.), (-.), clamp, lighten, darken, lerp
                  )
import Clay.Common
import Clay.Display
import Clay.Dynamic ( UserSelect, userSelect, selectText )
import Clay.Flexbox hiding ( flex, FlexWrap, flexWrap, nowrap, wrap, wrapReverse )
import Clay.Font
import Clay.Geometry
import Clay.Grid
import Clay.Pseudo hiding ( root )
import Clay.Selector hiding ( (**) )
import Clay.Size
import Clay.Stylesheet ( StyleM(..), Css
                       , key, prefixed, (-:)
                       , (?), (<?), (&)
                       , root
                       , pop
                       , MediaType(..), Feature(..)
                       , query, queryNot, queryOnly
                       , important, importUrl
                       )
import Clay.Text

import Roll20.CSS.Selectors

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show
