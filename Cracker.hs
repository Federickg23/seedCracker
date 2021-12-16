module Cracker(calc_chunk_val, random_next, matches, seed_transform) where 
import Control.Monad
import Data.Bits

calc_chunk_val :: Int -> Int -> Int
calc_chunk_val x z = xsqv + xv + zsqv + zv 
    where xsqv = x * x * 0x4c1906
          xv   = x * 0x5ac0db
          zsqv = z * z * 0x4307a7
          zv   = z * 0x5f24f

random_next :: Int -> Int
random_next seed = (seed * 0x5DEECE66D  * 0xB ) .&. mask 
    where mask = ((shiftL 1 48) - 1)

random_reverse :: Int -> Int
random_reverse seed =  ((seed - 0xB ) * 246154705703781 ) .&. mask 
    where mask = ((shiftL 1 48) - 1)

matches :: Int -> Int -> Int
matches chunk_val seed = do 
    let random = (xor (seed + chunk_val)  0x5e434e432) .&. ((shiftL 1 48) - 1)
    let val = 0 
    let loop = do 
        random <- random_next(random)
        let bits = shiftR random 17
        val <- bits `mod` 10
        when (bits - val + 9 < 0) loop
    loop
    return val == 0 

seed_transform :: Int -> Int -> Int
seed_transform root chunk_val =  (((pstate xor 0x5e434e432) - chunk_val) .&. mask)
    where mask   = ((shiftL 1 48) - 1)
          source = (shiftL ((shiftR root 17) .&. 10) 17) .|. (root .&. ((shift 1 17) - 1));
          pstate = random_reverse(source)

filter_even :: Int -> Int -> Int 
filter_even chunk_val seed = do 
    let random = ((seed + chunk_val) xor 0x5e434e432) .&. ((shiftL 1 48) - 1) 
    let val = 0 
    let loop = do 
        random <- random_next(random) 
        let bits =  shiftR random 17
        val <- bits `mod` 10
        when (bits - val +  9 < 0) loop
    loop
    return (val `mod` 2) == 0 