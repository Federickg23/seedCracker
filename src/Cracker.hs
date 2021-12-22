module Cracker(calcChunkVal, calcLowerBitSeries, expand48To64Random, calcSeq,
    calcParListBlocks, calcSeqNaive, calcParListBlocksNaive, calcParBufferNaive,
    calcParMonadBlocks, calcParMonadBlocksNaive) where 
import Data.Bits(Bits(shiftR, shiftL, xor, (.&.), (.|.)))
import Data.Int(Int32, Int64)
import Data.Word(Word32, Word64)
import Data.List(foldl')
import Control.Parallel.Strategies(using, parList, rdeepseq, parBuffer)
import Control.Monad.Par(runPar)
import Control.Monad.Par.Combinator(parMap)

-- adapted from / inspired by https://stackoverflow.com/q/43033099
concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' f = reverse . foldl' (flip ((++) . f)) []

mask48Bit :: Word64
mask48Bit = 1 `shiftL` 48 - 1
 
calcChunkVal :: (Int32, Int32) -> Int64
calcChunkVal (x, z) = xsqv + xv + zsqv + zv 
    where xsqv = fromIntegral (xw * xw * 0x4c1906)
          xw   = fromIntegral x :: Word32
          xv   = fromIntegral $ x * 0x5ac0db
          zsqv = fromIntegral (z * z) * 0x4307a7
          zv   = fromIntegral $ z * 0x5f24f

randomNext :: Word64 -> Word64
randomNext seed = (seed * 0x5deece66d + 0xb) .&. mask48Bit

randomReverse :: Word64 -> Word64
randomReverse seed = ((seed - 0xb) * 246154705703781) .&. mask48Bit

matches :: Int64 -> Int64 -> Bool
matches seed chunkVal = doShift random == 0 where
    random = fromIntegral ((seed + chunkVal) `xor` 0x5e434e432) .&. mask48Bit

checkEven :: Int64 -> Int64 -> Bool 
checkEven seed chunkVal = even $ doShift random where
    random = fromIntegral ((seed + chunkVal) `xor` 0x5e434e432) .&. mask48Bit

doShift :: Word64 -> Word64
doShift random
    | bits - val + 9 < 0 = doShift nextRandom
    | otherwise = val
    where nextRandom = randomNext random
          bits = nextRandom `shiftR` 17
          val = bits `mod` 10

calcLowerBitSeries :: [Int64] -> [Int64]
calcLowerBitSeries chunkVals = filter (flip all chunkVals . checkEven)
                                [0 .. 1 `shiftL` 18 - 1] :: [Int64]

calcSlimeSeedsNaive :: [Int64] -> [Int64] -> Int64 -> [Int64] 
calcSlimeSeedsNaive chunkVals seeds lowerBits
    = filter (flip all chunkVals . matches)
        $ map ((.|. lowerBits) . (`shiftL` 18)) seeds

calcSlimeSeedsParBuffer :: [Int64] -> [Int64] -> Int64 -> [Int64] 
calcSlimeSeedsParBuffer chunkVals seeds lowerBits
    = filter (flip all chunkVals . matches)
    (map ((.|. lowerBits) . (`shiftL` 18)) seeds `using` parBuffer 100 rdeepseq)
    `using` parBuffer 100 rdeepseq

calcSlimeSeeds :: [Int64] -> (Int64, Int64) -> Int64 -> [Int64] 
calcSlimeSeeds chunkVals (seed, endSeed) lowerBits
    | seed == endSeed = []
    | all (matches fullSeed) chunkVals = fullSeed : restSeeds
    | otherwise = restSeeds
    where fullSeed = (seed `shiftL` 18) .|. lowerBits
          restSeeds = calcSlimeSeeds chunkVals (seed + 1, endSeed) lowerBits

expand48To64Random :: Int64 -> [Int64]
expand48To64Random seed = map ((.|. lowerInt) . (`shiftL` 32) . subtract offset)
    $ filter ((upperPartial ==) . (.&. mask16Bit)) $ map
    (fromIntegral . (`shiftR` 16) . randomReverse . fromIntegral . (middle .|.))
    [0 .. mask16Bit]
    where mask16Bit = 1 `shiftL` 16 - 1
          lowerInt = seed .&. (1 `shiftL` 32 - 1)
          middle = lowerInt `shiftL` 16
          offset = (seed .&. (1 `shiftL` 31)) `shiftR` 31
          upperPartial = (seed `shiftR` 32 + offset) .&. mask16Bit

calcSeqNaive :: [Int64] -> [Int64] -> [Int64]
calcSeqNaive chunkVals
    = concatMap' (calcSlimeSeedsNaive chunkVals [0 .. 1 `shiftL` 30 - 1])

calcParBufferNaive :: [Int64] -> [Int64] -> [Int64]
calcParBufferNaive chunkVals
    = concatMap' (calcSlimeSeedsParBuffer chunkVals [0 .. 1 `shiftL` 30 - 1])

calcParListBlocksNaive :: Int64 -> [Int64] -> [Int64] -> [Int64]
calcParListBlocksNaive numBlocks chunkVals lowerBits
    = concat (map (flip concatMap' lowerBits . calcSlimeSeedsNaive chunkVals)
        blocks `using` parList rdeepseq)
    where
        step = 1 `shiftL` 30 `quot` numBlocks
        blocks = map (\i -> [i .. i + step - 1]) [0, step .. 1 `shiftL` 30 - 1]

calcParMonadBlocksNaive :: Int64 -> [Int64] -> [Int64] -> [Int64]
calcParMonadBlocksNaive numBlocks chunkVals lowerBits = concat $ runPar
    $ parMap (flip concatMap' lowerBits . calcSlimeSeedsNaive chunkVals) blocks
    where
        step = 1 `shiftL` 30 `quot` numBlocks
        blocks = map (\i -> [i .. i + step - 1]) [0, step .. 1 `shiftL` 30 - 1]

calcSeq :: [Int64] -> [Int64] -> [Int64]
calcSeq chunkVals
    = concatMap (calcSlimeSeeds chunkVals (0, 1 `shiftL` 30 - 1))

calcParListBlocks :: Int64 -> [Int64] -> [Int64] -> [Int64]
calcParListBlocks numBlocks chunkVals lowerBits
    = concat (map (flip concatMap lowerBits . calcSlimeSeeds chunkVals) blocks
        `using` parList rdeepseq)
    where
        step = 1 `shiftL` 30 `quot` numBlocks
        blocks = map (\i -> (i, i + step - 1)) [0, step .. 1 `shiftL` 30 - 1]

calcParMonadBlocks :: Int64 -> [Int64] -> [Int64] -> [Int64]
calcParMonadBlocks numBlocks chunkVals lowerBits = concat $ runPar
    $ parMap (flip concatMap lowerBits . calcSlimeSeeds chunkVals) blocks
    where
        step = 1 `shiftL` 30 `quot` numBlocks
        blocks = map (\i -> (i, i + step - 1)) [0, step .. 1 `shiftL` 30 - 1]