module Main ( main ) where

import Roll20.Tabbed ( tabbed )
import Renaissance.Sheet ( RenTab )

import qualified Clay as CSS
import qualified Lucid as HTML
import qualified Data.Text.Lazy.IO as TL


main :: IO ()
main = do
    HTML.renderToFile "sheet.html" html
    TL.writeFile "sheet.css" $ CSS.renderWith CSS.compact [] css
    where
        (html, css) = tabbed @RenTab
