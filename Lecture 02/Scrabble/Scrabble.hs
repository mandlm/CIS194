module Scrabble where

import Words
import Data.List

type Hand = [Char]

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) hand = let remainingHand = delete x hand in
  elem x hand && formableBy xs remainingHand
 
wordsFromList :: Hand -> [String] -> [String]
wordsFromList hand [] = []
wordsFromList hand (x:[]) 
  | formableBy x hand = [x]
  | otherwise = []
wordsFromList hand (x:xs) = wordsFromList hand [x] ++ wordsFromList hand xs

wordsFrom :: Hand -> [String]
wordsFrom hand = wordsFromList hand allWords

type Template = String

wordMatchesTemplate :: String -> Template -> Bool
wordMatchesTemplate [] [] = True
wordMatchesTemplate (w:ws) (t:ts)
  | t == '?' = wordMatchesTemplate ws ts
  | otherwise = w == t && wordMatchesTemplate ws ts
wordMatchesTemplate _ _ = False

removeTemplateChars :: String -> Template -> String
removeTemplateChars word [] = word
removeTemplateChars word (t:ts)
  | t == '?' = removeTemplateChars word ts
  | otherwise = removeTemplateChars (delete t word) ts

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate template hand word = let matchWord = removeTemplateChars word template in 
  formableBy matchWord hand && wordMatchesTemplate word template


