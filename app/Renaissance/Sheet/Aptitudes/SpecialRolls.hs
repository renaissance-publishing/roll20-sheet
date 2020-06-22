module Renaissance.Sheet.Aptitudes.SpecialRolls
    ( specialRollHtml, specialRolls, luckRoll
    ) where

import qualified Data.Text as T
import Lucid

import Roll20.Field
import Roll20.RollTemplate
import Renaissance.Fields.Global
import Renaissance.RollTemplates.SkillLike

--

data SpecialRoll = Scale RenField Integer T.Text
                 | Sum RenField RenField T.Text
                 deriving (Show, Eq)

specialRollShorthand :: SpecialRoll -> T.Text
specialRollShorthand sr =
    case sr of
        Scale f s _ -> T.concat [friendlyName f, " &times; ", T.pack (show s)]
        Sum l r _   -> T.concat [friendlyName l, " + ", friendlyName r]

friendlyName :: RenField -> T.Text
friendlyName (Apt apt _) = T.pack (show apt)
friendlyName (Derived d) = T.pack (show d)
friendlyName LuckTotal   = "Total Luck"
friendlyName f           = error $ "unknown friendly name: " ++ show f

specialRollCommand :: SpecialRoll -> Roll
specialRollCommand (Scale f s _) = Comment (Field f) (friendlyName f) * fromInteger s
specialRollCommand (Sum l r _)   = Comment (Field l) (friendlyName l) + Comment (Field r) (friendlyName r)

specialRollName :: SpecialRoll -> T.Text
specialRollName (Scale _ _ n) = n
specialRollName (Sum _ _ n)   = n

specialRoll :: SpecialRoll -> Roll
specialRoll sr =
    rollWithTemplate
        skillLikeRollTemplate
        (Field ShortCharacterName)
        (RawText $ specialRollName sr)
        epStyleDice
        (specialRollCommand sr - woundTraumaModifier + modifier)

specialRollHtml :: SpecialRoll -> SheetR ()
specialRollHtml sr = tr_ $ do
    td_ [class_ "sheet-center"] $
        rollButton (specialRoll sr) $
            toHtmlRaw $ specialRollShorthand sr
    th_ $
        toHtml $ specialRollName sr

specialRolls :: [SpecialRoll]
specialRolls =
    [ Sum (Apt WIL Total) (Apt SAV Total) "Resist Intimidation"
    , Scale (Apt WIL Total) 2 "Overcome Fear"
    , Sum (Apt SAV Total) (Apt INT Total) "Resist Social Manipulation"
    , Scale (Apt COG Total) 3 "Difficult Recall/Solve a Puzzle"
    , Scale (Apt INT Total) 3 "Make a Guess/Get a Hint/Difficult Use of Language"
    , Sum (Apt SOM Total) (Derived STR) "Feat of Strength"
    , Sum (Derived DUR) (Apt WIL Total) "Feat of Endurance"
    , Scale (Apt COO Total) 3 "React Quickly/Catch Something"
    ]

luckRoll :: Roll
luckRoll = specialRoll $ Scale LuckTotal 10 "Stroke of Luck"
