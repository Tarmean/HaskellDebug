{-# OPTIONS_GHC -ddump-prep  -ddump-to-file -dsuppress-all #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc

    ) where

import UI ( printValue )
                                                    
                                            
someFunc :: IO ()
someFunc = printValue (sieve [2..])

nthPrimeIdx :: Int
nthPrimeIdx = nthPrime - 1

nthPrime :: Int
nthPrime = 10001

sieve :: [Int] -> [Int]
sieve [] = [] -- added
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]