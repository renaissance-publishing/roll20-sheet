module Renaissance.Fields.Milestones ( MilestoneField(..) )where

import Roll20.Field

data MilestoneField = MilestoneName
                    | MilestoneDescription
                    deriving (Show, Eq)

instance RepeatingSection MilestoneField "milestones" where
    repeatingFieldType MilestoneName = TextF
    repeatingFieldType MilestoneDescription = TextareaF
