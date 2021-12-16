module Cracker(calcChunkVal, randomNext, matches) where 
import Control.Monad
import Data.Bits

mask48Bit = shiftL 1 48 - 1

calcChunkVal :: Int -> Int -> Int
calcChunkVal x z = xsqv + xv + zsqv + zv 
    where xsqv = x * x * 0x4c1906
          xv = x * 0x5ac0db
          zsqv = z * z * 0x4307a7
          zv = z * 0x5f24f

randomNext :: Int -> Int
randomNext seed = (seed * 0x5DEECE66D  * 0xB ) .&. mask48Bit

randomReverse :: Int -> Int
randomReverse seed =  ((seed - 0xB ) * 246154705703781 ) .&. mask48Bit

matches :: Int -> Int -> Int
matches chunk_val seed = do 
    let random = ((seed + chunk_val) `xor`  0x5e434e432) .&. mask48Bit
    let val = 0 
    let loop = do 
        let random = randomNext random
        let bits = shiftR random 17
        let val = bits `mod` 10
        when (bits - val + 9 < 0) loop
    loop
    return val == 0 


filterEven :: Int -> Int -> Int 
filterEven chunk_val seed = do 
    let random = ((seed + chunk_val) `xor` 0x5e434e432) .&. mask48Bit
    let val = 0 
    let loop = do 
        let random = randomNext random 
        let bits =  shiftR random 17
        let val = bits `mod` 10
        when (bits - val +  9 < 0) loop
    loop
    return (val `mod` 2) == 0