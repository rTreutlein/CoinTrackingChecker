{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Csv
import Data.Time
import Debug.Trace

data Tx = Tx
     { typ    :: !String
     , tin    :: !Float
     , inCur  :: !String
     , tout   :: !Float
     , outCur :: !String
     , fee    :: !Float
     , feeCur :: !String
     , date   :: !UTCTime
     , idx    :: !Int
     }
     deriving (Show,Eq)

instance Ord Tx where
    tx1 <= tx2 = if (date tx1) /= (date tx2)
                   then  (date tx1) <= (date tx2)
                   else if (typ tx1) == "Auszahlung"
                           then True
                           else False

instance FromField UTCTime where
    parseField = parseTimeM True defaultTimeLocale "%d.%m.%Y %H:%M"
                 . filter (/= '\"')
                 . show


instance FromNamedRecord Tx where
    parseNamedRecord r = Tx <$> r .: "Typ"
                            <*> r .: "Kauf"
                            <*> r .: "Kcur"
                            <*> r .: "Verkauf"
                            <*> r .: "Vcur"
                            <*> r .: "Gebuhr"
                            <*> r .: "Gcur"
                            <*> r .: "Datum"
                            <*> r .: "Index"


