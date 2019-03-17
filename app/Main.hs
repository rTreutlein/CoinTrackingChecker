{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib

import Control.Applicative
import Data.Foldable
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V
import qualified Data.Map as M

main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> print $ foldl check M.empty $ sort $ toList v


fm = flip (-)

check :: M.Map String Float -> Tx -> M.Map String Float
check m tx@Tx{..} = case typ of
                    "Mining" -> M.insertWith (+) inCur tin m
                    "Einnahme" -> M.insertWith (+) inCur tin m
                    "Ausgabe" -> M.insertWith fm outCur tout m
                    "Einzahlung" ->
                        let k = 'A':inCur
                            m1 = M.insertWith fm k tin m
                            m2 = M.insertWith (+) inCur tin m1
                        in checkA tx m2
                    "Auszahlung" ->
                        let k = 'A':outCur
                            m1 = M.insertWith fm outCur tout m
                            m2 = M.insertWith (+) k tout m1
                            m3 = M.insertWith fm k fee m2
                        in checkA tx m3
                    "Trade" ->
                        let m1 = M.insertWith fm outCur tout m
                            m2 = M.insertWith (+) inCur tin m1
                            --already subtracted
                            --m3 = M.insertWith fm feeCur fee m2
                        in checkA tx m2
                    a -> error a


checkA :: Tx -> M.Map String Float -> M.Map String Float
checkA tx m = if any (< 0) $ M.filterWithKey filterf m
                 then error $ show tx ++ "\n" ++ show m
                 else m

filterf ('A':_) _ = True
filterf _ _ = False

