module Main where

import CarRental (solve, Policy, Values)

main :: IO ()
main = print $ fst solve
