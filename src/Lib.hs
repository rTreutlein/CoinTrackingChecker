{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib where

import Data.Csv
import Data.Time
import Data.Scientific
import Data.ByteString.Char8 (pack)
import Debug.Trace
import Text.Printf

data Tx = Tx
     { typ    :: !String
     , tin    :: !Scientific
     , inCur  :: !String
     , tout   :: !Scientific
     , outCur :: !String
     , fee    :: !Scientific
     , feeCur :: !String
     , sid    :: !String
     , comment:: !String
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
                            <*> r .: "Kommentar"
                            <*> r .: "Datum"
                            <*> r .: "Index"

instance ToField UTCTime where
    toField = pack . show

instance ToNamedRecord Tx where
    toNamedRecord tx@Tx{..} = namedRecord ["Typ" .= typ
                                          ,"Kauf" .= tin
                                          ,"Kcur" .= inCur
                                          ,"Verkauf" .= tout
                                          ,"Vcur" .= outCur
                                          ,"Gebuhr" .= fee
                                          ,"Gcur" .= feeCur
                                          ,"ID" .= sid
                                          ,"Kommentar" .= comment
                                          ,"Datum" .= date
                                          ,"Index" .= idx
                                          ]

instance Show Tx where
    show tx@Tx{..} =
          case typ of
              "Einzahlung" -> s_spc ++ ">>>" ++ s_in  ++ rest
              "Auszahlung" -> s_out ++ ">>>" ++ s_spc ++ rest
              "Trade" ->      s_out ++ ">>>" ++ s_in  ++ rest
              "Einnahme" ->   s_spc ++ ">+>" ++ s_in  ++ rest
              "Airdrop" ->    s_spc ++ ">+>" ++ s_in  ++ rest
              "Mining" ->     s_spc ++ ">+>" ++ s_in  ++ rest
              "Masternode" -> s_spc ++ ">+>" ++ s_in  ++ rest
              "Ausgabe" ->    s_out ++ ">->" ++ s_spc ++ rest
              "Sonstige Gebühr" ->     s_out ++ ">->" ++ s_spc ++ rest
              "Dividenden Einnahme" -> s_spc ++ ">+>" ++ s_in  ++ rest
        where s_in  = formatScientific Generic (Just 2) tin ++ padL 4 inCur
              s_out = formatScientific Generic (Just 2) tout ++ padL 4 outCur
              s_spc = "         "
              rest = show date ++ " " ++ sid


padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s
