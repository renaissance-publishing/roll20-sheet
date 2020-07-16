module Renaissance.Fields.Global
    ( Aptitude(..), AptitudeRow(..)
    , VitalStat(..)
    , DerivedStat(..)
    , RenField(..)
    ) where

import qualified Data.Text as T
import Data.Universe.Class
import Language.Javascript.JMacro
import Lucid

import Roll20.Field hiding ( field )
import Roll20.SheetWorker

data Aptitude = COG | COO | INT | SAV | SOM | WIL
              deriving stock (Show, Eq, Ord, Enum, Bounded)
              deriving anyclass (Universe, Finite)

instance ToHtml Aptitude where
    toHtml    = toHtml    . tshow
    toHtmlRaw = toHtmlRaw . tshow

data AptitudeRow = Base | Temp | Total
              deriving stock (Show, Eq, Ord, Enum, Bounded)
              deriving anyclass (Universe, Finite)

instance ToHtml AptitudeRow where
    toHtml    = toHtml    . tshow
    toHtmlRaw = toHtmlRaw . tshow

data VitalStat = Name
               | Gender
               | Pronouns
               | Race
               | Age
               | Height
               | Weight
               | Hair
               | Eyes
               | Skin
               | Background
               | Parents
               deriving stock (Show, Eq, Ord, Enum, Bounded)
               deriving anyclass (Universe, Finite)

instance ToHtml VitalStat where
    toHtml    = toHtml    . tshow
    toHtmlRaw = toHtmlRaw . tshow

data DerivedStat = CurrentWounds
                 | IgnoredWounds
                 | CurrentTrauma
                 | IgnoredTrauma
                 | DUR
                 | LUC
                 | WT
                 | TT
                 | DR
                 | IR
                 | DB
                 | INIT
                 | STR
                 deriving stock (Show, Eq, Ord, Enum, Bounded)
                 deriving anyclass (Universe, Finite)

derivedStatName :: DerivedStat -> T.Text
derivedStatName CurrentWounds = "Current Wounds"
derivedStatName IgnoredWounds = "Ignored Wounds"
derivedStatName CurrentTrauma = "Current Trauma"
derivedStatName IgnoredTrauma = "Ignored Trauma"
derivedStatName derived       = tshow derived

instance ToHtml DerivedStat where
    toHtml    = toHtml    . derivedStatName
    toHtmlRaw = toHtmlRaw . derivedStatName

data RenField = Apt Aptitude AptitudeRow
              | Vital VitalStat
              | ShortCharacterName
              | ShortDescription
              | Description
              | DetailedBackground
              | MotivationType Int
              | MotivationDesc Int
              | Derived DerivedStat
              | LuckCurrent
              | LuckTotal
              | Movement
              | Senses
              | WealthLevel
              | WealthCurrent Int
              | WealthTotal Int
              deriving (Show, Eq)

instance SheetSection RenField where
    fieldName (Apt apt row) = T.concat ["Apt_", tshow apt, "_", tshow row]
    fieldName (Vital vital) = "Vital_" <> tshow vital
    fieldName (MotivationType n) = "MotivationType_" <> tshow n
    fieldName (MotivationDesc n) = "MotivationDesc_" <> tshow n
    fieldName (Derived derived) = "Derived_" <> tshow derived
    fieldName (WealthCurrent n) = "WealthCurrent_" <> tshow n
    fieldName (WealthTotal n) = "WealthTotal_" <> tshow n
    fieldName field = tshow field

    fieldType (Vital _) = TextF
    fieldType ShortCharacterName = TextF
    fieldType ShortDescription = TextF
    fieldType Description = TextareaF
    fieldType DetailedBackground = TextareaF
    fieldType (MotivationType _) = TextF
    fieldType (MotivationDesc _) = TextF
    fieldType Movement = TextF
    fieldType Senses = TextF
    fieldType _ = NumberF

    fieldDefault WealthLevel         = Just "1"
    fieldDefault field
        | NumberF <- fieldType field = Just "0"
        | otherwise                  = Nothing

    fieldWorker dst@(Apt apt Total) = Just $
        autocalculate dst (Apt apt Base, Apt apt Temp) (+)
    fieldWorker dst@(Derived LUC) = Just $
        autocalculate dst (Apt WIL Total) (* 2)
    fieldWorker dst@(Derived WT) = Just $
        autocalculate dst (Derived DUR) (floorDiv 5)
    fieldWorker dst@(Derived TT) = Just $
        autocalculate dst (Derived LUC) (floorDiv 5)
    fieldWorker dst@(Derived DR) = Just $
        autocalculate dst (Derived DUR) (* 2)
    fieldWorker dst@(Derived IR) = Just $
        autocalculate dst (Derived LUC) (* 2)
    fieldWorker dst@(Derived DB) = Just $
        autocalculate dst (Apt SOM Total, Derived STR) (floorDiv2 10)
    fieldWorker dst@(Derived INIT) = Just $
        autocalculate dst (Apt INT Total, Apt COO Total) (floorDiv2 5)
    fieldWorker _ = Nothing

--

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

floorDiv :: JExpr -> JExpr -> JExpr
floorDiv a x = floor' $ x / a

floorDiv2 :: JExpr -> JExpr -> JExpr -> JExpr
floorDiv2 a x y = floor' $ (x + y) / a
