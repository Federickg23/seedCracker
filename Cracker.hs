module Cracker(calcChunkVal, randomNext, matches) where 
import Data.Bits(Bits(shiftR, shiftL, xor, (.&.)))
import Data.Int(Int64)
import Data.Word(Word64)

mask48Bit :: Word64
mask48Bit = 1 `shiftL` 48 - 1

calcChunkVal :: Int -> Int -> Int64
calcChunkVal x z = fromIntegral $ xsqv + xv + zsqv + zv 
    where xsqv = x * x * 0x4c1906
          xv   = x * 0x5ac0db
          zsqv = z * z * 0x4307a7
          zv   = z * 0x5f24f

randomNext :: Word64 -> Word64
randomNext seed = (seed * 0x5deece66d + 0xb) .&. mask48Bit

randomReverse :: Word64 -> Word64
randomReverse seed = ((seed - 0xb) * 246154705703781) .&. mask48Bit

matches :: Int64 -> Int64 -> Bool
matches seed chunkVal = doShift random == 0 where
    random = fromIntegral ((seed + chunkVal) `xor` 0x5e434e432) .&. mask48Bit

filterEven :: Int64 -> Int64 -> Bool 
filterEven seed chunkVal = even $ doShift random where
    random = fromIntegral ((seed + chunkVal) `xor` 0x5e434e432) .&. mask48Bit

doShift :: Word64 -> Word64
doShift random
    | bits - val + 9 < 0 = doShift nextRandom
    | otherwise = val
    where nextRandom = randomNext random
          bits = nextRandom `shiftR` 17
          val = bits `mod` 10

calcLowerBitSeries :: [Int64] -> [Int64]
calcLowerBitSeries chunkVals = filter (\seed -> all (filterEven seed) chunkVals)
                                [0..262143] :: [Int64]