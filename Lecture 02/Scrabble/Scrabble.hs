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

wordsFromListFittingTemplate :: Template -> Hand -> [String] -> [String]
wordsFromListFittingTemplate _ _ [] = []
wordsFromListFittingTemplate template hand (x:xs)
  | wordFitsTemplate template hand x = x : wordsFromListFittingTemplate template hand xs
  | otherwise = wordsFromListFittingTemplate template hand xs

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = wordsFromListFittingTemplate template hand allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord (x:xs) = scrabbleValue x + scrabbleValueWord xs

bestScrabbleWordValueAcc :: [String] -> Int -> Int
bestScrabbleWordValueAcc [] acc = acc
bestScrabbleWordValueAcc (x:xs) acc = let currentWordValue = scrabbleValueWord x in
  let bestValue = max currentWordValue acc in 
    bestScrabbleWordValueAcc xs bestValue

bestScrabbleWordValue :: [String] -> Int
bestScrabbleWordValue words = bestScrabbleWordValueAcc words 0

bestWords :: [String] -> [String]
bestWords [] = []
bestWords (x:xs)
  | scrabbleValueWord(x) == maxWordValue = x : bestWords xs
  | otherwise = bestWords xs
  where 
    maxWordValue = bestScrabbleWordValue (x:xs)

type STemplate = String

scrabbleValueTemplateAcc :: Int -> Int -> STemplate -> String -> Int
scrabbleValueTemplateAcc sum mult [] [] = sum * mult
scrabbleValueTemplateAcc sum mult (t:ts) (w:ws)
  | t == '?' = scrabbleValueTemplateAcc (sum + charValue) mult ts ws
  | t == 'D' = scrabbleValueTemplateAcc (sum + (charValue * 2)) mult ts ws
  | t == 'T' = scrabbleValueTemplateAcc (sum + (charValue * 3)) mult ts ws
  | t == '2' = scrabbleValueTemplateAcc (sum + charValue) (mult * 2) ts ws
  | t == '3' = scrabbleValueTemplateAcc (sum + charValue) (mult * 3) ts ws
  | otherwise = scrabbleValueTemplateAcc (sum + charValue) mult ts ws
  where charValue = scrabbleValue w

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate template word = scrabbleValueTemplateAcc 0 1 template word








