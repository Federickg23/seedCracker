module Main where

import Cracker
import System.Environment(getArgs)
import Control.Monad(unless)
import System.Exit(die)

main :: IO ()
main = do
    args <- getArgs
    unless (length args <= 3 && head args `elem` ["par", "seq"])
        $ die "Usage: (par|seq|parBuffer) <num-blocks> [naive]"
    let numBlocks = case args of
            [_, num] -> read num
            _ -> 128
    let chunkCoords = [(-1, -4), (15, -7), (-11, -4), (18, -2), (-3, 4), (-15, -10), (-16, 4), (-17, -3), (-9, -10), (-8, -13), (-19, -2), (-8, -14), (-9, -14), (-178, -99), (-91, -51), (-10, 7), (61, 61), (66, 66)]
    let chunkVals = map calcChunkVal chunkCoords
    putStrLn "Calculated the following chunk values from the provided \
        \chunk coordinates:"
    print chunkVals
    let lowerBits = calcLowerBitSeries chunkVals
    putStrLn "Calculated the following valid lower 18 bits:"
    print lowerBits
    let seeds = case args of
            ["par", _, "naive"] ->
                calcParListBlocksNaive numBlocks chunkVals lowerBits
            ["seq", _, "naive"] -> calcSeqNaive chunkVals lowerBits
            "par" : _ -> calcParListBlocks numBlocks chunkVals lowerBits
            "seq" : _ -> calcSeq chunkVals lowerBits
            "parBuffer" : _ -> calcParBufferNaive chunkVals lowerBits
            _ -> [0]
    putStrLn "Calculated the following valid 40-bit seeds:"
    print seeds
    let fullSeeds = concatMap expand48To64Random seeds
    putStrLn "Calculated the following valid 64-bit randomly generatable seeds:"
    print fullSeeds
