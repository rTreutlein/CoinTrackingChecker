{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib where

import Data.Csv
import Data.Time
import Debug.Trace
import Text.Printf

data Tx = Tx
     { typ    :: !String
     , tin    :: !Float
     , inCur  :: !String
     , tout   :: !Float
     , outCur :: !String
     , fee    :: !Float
     , feeCur :: !String
     , sid    :: !String
     , date   :: !UTCTime
     , idx    :: !Int
     }
     deriving Eq

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
                            <*> r .: "ID"
                            <*> r .: "Datum"
                            <*> r .: "Index"
instance Show Tx where
    show tx@Tx{..} =
          case typ of
              "Einzahlung" -> s_spc ++ ">>>" ++ s_in  ++ rest
              "Auszahlung" -> s_out ++ ">>>" ++ s_spc ++ rest
              "Trade" ->      s_out ++ ">>>" ++ s_in  ++ rest
              "Einnahme" ->   s_spc ++ ">+>" ++ s_in  ++ rest
              "Mining" ->     s_spc ++ ">+>" ++ s_in  ++ rest
              "Masternode" -> s_spc ++ ">+>" ++ s_in  ++ rest
              "Ausgabe" ->    s_out ++ ">->" ++ s_spc ++ rest
              "Sonstige Gebühr" ->     s_out ++ ">->" ++ s_spc ++ rest
              "Dividenden Einnahme" -> s_spc ++ ">+>" ++ s_in  ++ rest
        where s_in  = printf "%.2f " tin ++ padL 4 inCur
              s_out = printf "%.2f " tout ++ padL 4 outCur
              s_spc = "         "
              rest = show date ++ " " ++ sid


padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s
