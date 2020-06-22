module Renaissance.Sheet ( RenTab(..) ) where

import Data.Universe.Class ( Universe, Finite )
import qualified Data.Text as T

import Roll20.Tabbed

import Renaissance.Sheet.Global ( global )
import Renaissance.Sheet.Vitals ( vitals )
import Renaissance.Sheet.Aptitudes ( aptitudesAndDerived )
import Renaissance.Sheet.Skills ( skills )
import Renaissance.Sheet.ClassesTraitsPowers ( classesTraitsPowers )
import Renaissance.Sheet.Items ( itemsTab )

data RenTab = Vitals
            | Aptitudes
            | Skills
            | ClassesTraitsPowers
            | Items
            deriving stock (Show, Eq, Ord, Enum, Bounded)
            deriving anyclass (Universe, Finite)

instance TabbedSheet RenTab where
    tabName Vitals              = "Vital Statistics"
    tabName Aptitudes           = "Aptitudes and Derived Statistics"
    tabName Skills              = "Skills and Languages"
    tabName ClassesTraitsPowers = "Classes, Traits, and Powers"
    tabName Items               = "Items"

    tabSlug = T.pack . show

    tabBody Vitals = vitals
    tabBody Aptitudes = aptitudesAndDerived
    tabBody Skills = skills
    tabBody ClassesTraitsPowers = classesTraitsPowers
    tabBody Items = itemsTab

    sheetGlobal = global
