{-|
This module simply reexports the named CSS colors defined in "Clay.Color", so
that they can easily be imported qualified in a stylesheet that uses
"Roll20.CSS".
-}
module Roll20.CSS.Colors ( module Clay.Color ) where

import Clay.Color hiding ( Color
                         , Rgba, rgba, rgb, Hsla, hsla, hsl, grayish, Other
                         , transparent
                         , setR, setG, setB, setA
                         , toRgba, toHsla
                         , (*.), (+.), (-.), clamp, lighten, darken, lerp
                         , parse
                         )
