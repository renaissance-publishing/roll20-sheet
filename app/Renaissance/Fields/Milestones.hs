module Renaissance.Fields.Milestones ( MilestoneField(..) )where

import Roll20.Field

data MilestoneField = MilestoneName
                    | MilestoneDescription
                    deriving (Show, Eq)

instance RepeatingSection MilestoneField where
    type RepeatingSectionName MilestoneField = "milestones"

    repeatingFieldType MilestoneName = TextF
    repeatingFieldType MilestoneDescription = TextareaF
