module Cracker(calcChunkVal, calcLowerBitSeries, calcSlimeSeedsSeq,
    expand48To64Random) where 
import Data.Bits(Bits(shiftR, shiftL, xor, (.&.), (.|.)))
import Data.Int(Int32, Int64)
import Data.Word(Word32, Word64)

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

calcSlimeSeedsSeq :: [Int64] -> Int64 -> [Int64] 
calcSlimeSeedsSeq chunkVals lowerBits = filter (flip all chunkVals . matches)
    $ map ((.|. lowerBits) . (`shiftL` 18)) [0 .. 1 `shiftL` 30 - 1] :: [Int64]

expand48To64Random :: Int64 -> [Int64]
expand48To64Random seed = map ((.|. lowerInt) . (`shiftL` 32) . subtract offset)
    $ filter isPossible [0 .. mask16Bit] where
    mask16Bit = 1 `shiftL` 16 - 1
    lowerInt = seed .&. (1 `shiftL` 32 - 1)
    middle = lowerInt `shiftL` 16
    offset = (seed .&. (1 `shiftL` 31)) `shiftR` 31
    upperPartial = (seed `shiftR` 32 + offset) .&. mask16Bit
    isPossible upperBits = (lastOutput .&. mask16Bit) == upperPartial where
        lastOutput = fromIntegral
            $ randomReverse (fromIntegral $ middle .|. upperBits) `shiftR` 16