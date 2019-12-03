module Lib where

import           Data.Monoid                    ( Sum(Sum)
                                                , getSum
                                                )

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

fuelForFuel :: Int -> Int
fuelForFuel x
  | requiredFuel < 0 = 0
  | otherwise = requiredFuel + fuelForFuel requiredFuel
    where requiredFuel = fuel x

totalFuel :: Int -> Int
totalFuel x = payloadFuel + fuelForFuel payloadFuel
  where payloadFuel = fuel x
