module Main where

import Cracker
import System.Environment(getArgs)
import Control.Monad(unless)
import System.Exit(die)
import Data.List(intercalate)
import Data.Ix(inRange)

main :: IO ()
main = do
    let types = ["seq", "parList", "parBuffer", "parMonad"]
    args <- getArgs
    unless (inRange (1, 4) (length args) && args !! 1 `elem` types) $
        die $ "Usage: <filename> (" ++ intercalate "|" types ++
            ") <num-blocks> [naive]"
    let numBlocks = case args of
            _ : _ : num : _ -> read num
            _ -> 128
    chunkCoords <- case args of
        filename : _ : _ -> do
            text <- readFile filename
            return $ map (((\[x, y] -> (x, y)) . map read) . words) (lines text)
        _ -> return []
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
            _ : "parList" : _  ->
                calcParListBlocks numBlocks chunkVals lowerBits
            _ : "seq" : _ -> calcSeq chunkVals lowerBits
            _ : "parBuffer" : _  -> calcParBufferNaive chunkVals lowerBits
            _ : "parMonad" : _ ->
                calcParMonadBlocks numBlocks chunkVals lowerBits
            _ -> [0]
    putStrLn "Calculated the following valid 40-bit seeds:"
    print seeds
    let fullSeeds = concatMap expand48To64Random seeds
    putStrLn "Calculated the following valid 64-bit randomly generatable seeds:"
    print fullSeeds