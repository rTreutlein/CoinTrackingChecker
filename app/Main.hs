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

import Debug.Trace

main :: IO ()
main = do
    csvData <- BL.readFile "ndata.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> do
            let lst = sort $ toList v
            writeFile "inputs.txt" ""
            fixed <- checkConsistency lst
            print fixed


checkConsistency :: [Tx] -> IO [Tx]
checkConsistency (tx@Tx{..}:txs) = case typ of
                                   "Mining" -> pure.(tx:) =<< checkConsistency txs
                                   "Masternode" -> pure.(tx:) =<< checkConsistency txs
                                   "Einnahme" -> pure.(tx:) =<< checkConsistency txs
                                   "Ausgabe" -> pure.(tx:) =<< checkConsistency txs
                                   "Trade" -> pure.(tx:) =<< checkConsistency txs
                                   "Dividenden Einnahme" -> pure.(tx:) =<< checkConsistency txs
                                   "Sonstige Gebühr" -> pure.(tx:) =<< checkConsistency txs
                                   "Einzahlung" -> do
                                       print tx
                                       (res,ntxs) <- findMatches [tx] [] txs
                                       remainder <- checkConsistency ntxs
                                       pure (res ++ remainder)
                                   "Auszahlung" -> do
                                       print tx
                                       (res,ntxs) <- findMatches [tx] [] txs
                                       remainder <- checkConsistency ntxs
                                       pure (res ++ remainder)

findMatches :: [Tx] -> [Tx] -> [Tx] -> IO ([Tx],[Tx])
findMatches found other (tx:txs) = do
    let (ftx:ftxs) = found
    case date tx == date ftx || (sid tx == sid ftx && sid tx /= "")  of
        True -> do
            print tx
            findMatches (tx:found) other txs
        False -> do
            putStrLn "?"
            print tx
            putStrLn "j to Accept, s to Skip, f to finish"
            answer <- getLine
            appendFile "inputs.txt" (answer ++ "\n")
            case answer of
                "j" -> findMatches (tx:found) other txs
                "s" -> findMatches found (tx:other) txs
                "f" -> do
                    print $ length found
                    checked <- fixMatches found
                    pure (checked, tx:other++txs)
                _   -> error "Aborted"

fixMatches :: [Tx] -> IO [Tx]
fixMatches txs = do
    let (inp, out, fee, ctxs) = foldl calcFee (0,0,0,[]) txs
        diff = inp - (out + fee)
        tx = head ctxs
        ntxs = feeTx fee tx : ctxs
    case diff of
        0 -> pure ntxs
        _ -> do
            putStrLn $ "Diff is: " ++ show diff
            putStrLn $ "Fee is: " ++ show fee
            putStrLn $ "a To use Diff as Fee, f {fee} to add custom"
            input <- getLine
            case words input of
                ("a":[])      -> pure (feeTx diff tx : txs)
                ("f":sfee:[]) -> pure (feeTx (read sfee) tx:txs)
                _             -> error "Aborted"


feeTx :: Float -> Tx -> Tx
feeTx f tx = Tx { typ    = "Ausgabe"
                , tin    = 0
                , inCur  = ""
                , tout   = 0
                , outCur = ""
                , fee    = f
                , feeCur = cur
                , sid    = sid tx ++ "-_-" ++ show f
                , date   = date tx
                , idx    = idx tx
                }
    where cur = case typ tx of
                    "Einzahlung" -> inCur tx
                    "Auszahlung" -> outCur tx

calcFee :: (Float,Float,Float,[Tx]) -> Tx -> (Float,Float,Float,[Tx])
calcFee (inp,out,tfee,txs) tx@Tx{..} = case typ of
    "Einzahlung" -> (inp+tin, out     , tfee + fee, (setFeeNull tx):txs)
    "Auszahlung" -> (inp    , out+tout, tfee + fee, (setFeeNull tx):txs)
    "Sonstige Gebühr" -> (inp + tout, out, tfee + tout, txs)
    other -> trace other (inp,out,tfee,txs)

setFeeNull :: Tx -> Tx
setFeeNull tx = tx { fee = 0, feeCur = ""}

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


