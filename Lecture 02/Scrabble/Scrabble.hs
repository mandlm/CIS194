module Scrabble where

import Words
import Data.List

type Hand = [Char]

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) hand = let remainingHand = delete x hand in
  elem x hand && formableBy xs remainingHand
 
wordsFrom :: Hand -> [String]
wordsFrom hand = wordsFromList hand allWords

wordsFromList :: Hand -> [String] -> [String]
wordsFromList hand [] = []
wordsFromList hand (x:[]) 
  | formableBy x hand = [x]
  | otherwise = []
wordsFromList hand (x:xs) = wordsFromList hand [x] ++ wordsFromList hand xs

