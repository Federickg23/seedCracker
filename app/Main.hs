module Main where

import Cracker
import Data.List(foldl')

-- adapted from / inspired by https://stackoverflow.com/q/43033099
concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' f = reverse . foldl' (flip ((++) . f)) []

main :: IO ()
main = do
    let chunkCoords = [(-1, -4), (15, -7), (-11, -4), (18, -2), (-3, 4), (-15, -10), (-16, 4), (-17, -3), (-9, -10), (-8, -13), (-19, -2), (-8, -14), (-9, -14), (-178, -99), (-91, -51), (-10, 7), (61, 61), (66, 66)]
    let chunkVals = map calcChunkVal chunkCoords --[67766623, 1423843817, 606747553, 1739683068, 98886225, 1468282775, 1253391356, 1378541357, 785819993, 1008925156, 1704145715, 1127142962, 1205976765, 45351056996, 13508566847, 657216746, 18109735831, 19802763400] --map calcChunkVal chunkCoords
    print chunkVals
    let lowerBits = calcLowerBitSeries chunkVals
    print lowerBits
    let seeds = concatMap' (calcSlimeSeedsSeq chunkVals) lowerBits
    print seeds
    let fullSeeds = concatMap expand48To64Random seeds
    print fullSeeds
