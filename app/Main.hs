module Main where

import Cracker
import System.Environment(getArgs)
import Control.Monad(unless)
import System.Exit(die)
import Data.List(intercalate)
import Data.Int (Int32)

main :: IO ()
main = do
    let types = ["seq", "parList", "parBuffer", "parMonad"]
    args <- getArgs
    unless (length args ==0 || length args == 1 || (length args <= 3 && head args `elem` types ) ) $
        die $ "Usage: <filename> (" ++ (intercalate "|" types) ++ ") <num-blocks> [naive]"
    let numBlocks = case args of
            [_, num] -> read num
            _ -> 128
    chunkCoords <- case args of
            [filename, _] -> do
                    text <- readFile filename
                    let fileLines = map read  . lines $ text
                    return $ map parseLine fileLines
            _ -> return [(-1, -4), (15, -7), (-11, -4), (18, -2), (-3, 4), (-15, -10), (-16, 4), (-17, -3), (-9, -10), (-8, -13), (-19, -2), (-8, -14), (-9, -14), (-178, -99), (-91, -51), (-10, 7), (61, 61), (66, 66)]
    let chunkVals = map calcChunkVal chunkCoords
    putStrLn "Calculated the following chunk values from the provided \
        \chunk coordinates:"
    print chunkVals
    let lowerBits = calcLowerBitSeries chunkVals
    putStrLn "Calculated the following valid lower 18 bits:"
    print lowerBits
    let seeds = case args of
            [_, "parList", _, "naive"] ->
                calcParListBlocksNaive numBlocks chunkVals lowerBits
            [_, "seq", _, "naive"] -> calcSeqNaive chunkVals lowerBits
            [_, "parList"]  -> calcParListBlocks numBlocks chunkVals lowerBits
            [_, "seq"] -> calcSeq chunkVals lowerBits
            [_, "parBuffer"]  -> calcParBufferNaive chunkVals lowerBits
            [_, "parMonad"] -> calcParMonadBlocks numBlocks chunkVals lowerBits
            _ -> [0]
    putStrLn "Calculated the following valid 40-bit seeds:"
    print seeds
    let fullSeeds = concatMap expand48To64Random seeds
    putStrLn "Calculated the following valid 64-bit randomly generatable seeds:"
    print fullSeeds

parseLine :: String -> (Int32, Int32)
parseLine line = (n, head rest) 
    where  [(n, rest)] = read line
    