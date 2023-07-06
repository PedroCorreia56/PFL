module Permutations (
    myPermutations,
    myPermutations2
) where

import Data.List

myPermutations ::(Eq a ) => [a] -> [[a]]
myPermutations [] = [[]]
myPermutations  l = [h:t | h <- l, t <- myPermutations (delete h l)]


remove2 :: (Eq a ) => a -> [a] -> [a]
remove2 _ [] = []
remove2 a (x:xs)
    | a==x = xs
    | otherwise = x:(remove2 a xs)

myPermutations2 ::(Eq a ) => [a] -> [[a]]
myPermutations2 [] = [[]]
myPermutations2 l = [ h:t | h<-l , t <- myPermutations2(remove2 h l) ]

