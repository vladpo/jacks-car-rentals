module Main where

import CarRental ( solve
                 , Policy
                 , Values
                 , solveValueIteration
                 )

--main :: IO ()
--main = print $ fst solve

main :: IO ()
main = print solveValueIteration
