--  File     : Proj1.hs
--  Author   : Can Cui (Student Number: 765821)
--  Purpose  : An implementation of functions such as feedback,initialGuess,nextGuess 
--             and a self-defined type GameState which is used to store a list of possible answers.

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List


-----------------------------------Feedback function------------------------------------------
-- | This function compares answer and guess, then returns 5 feedbacks:
--   1.c_cards: the number of correct cards (both rank and suit are correct) in guess
--   2.l_ranks: the number of cards in answer whose ranks are lower than the lowest rank in guess
--   3.c_ranks: the number of correct ranks in guess
--   4.h_ranks: the number of cards in answer whose ranks are higher than the highest rank in guess
--   5.c_suits: the number of correct suits in guess
feedback::[Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback answer guess = (c_cards,l_ranks,c_ranks,h_ranks,c_suits) 
  where 
    c_cards = numOfSameItem (sort answer) (sort guess)
    c_ranks = numOfSameItem (sort (map rank answer)) (sort (map rank guess))
    c_suits = numOfSameItem (sort (map suit answer)) (sort (map suit guess))
    l_ranks = numOfLower (map rank answer) (minimum  (map rank guess))
    h_ranks = numOfHiger (map rank answer) (maximum  (map rank guess))


--get the number of the same items of two sorted list 
numOfSameItem::(Eq t, Ord t)=> [t] -> [t] -> Int
numOfSameItem [] _  = 0
numOfSameItem _ []  = 0
numOfSameItem (x:xs) (y:ys)
  | x==y  =1 + numOfSameItem xs ys
  | x>y   =numOfSameItem (x:xs) ys
  | x<y   =numOfSameItem (xs) (y:ys) 


--get the number of cards whose rank is lower than the lowest 
numOfLower::[Rank] -> Rank -> Int
numOfLower [] _ = 0
numOfLower (x:xs) lowest
  | lowest > x  = 1 + numOfLower xs lowest
  | otherwise   = numOfLower xs lowest


--get the number of cards whose rank is higher than the highest 
numOfHiger::[Rank] -> Rank -> Int
numOfHiger [] _ = 0
numOfHiger (x:xs) highest
  | highest < x  = 1 + numOfHiger xs highest
  | otherwise   = numOfHiger xs highest

---------------------------------------Guess Functions-------------------------------------------------
-- GameState is a type used to store a list of remaining possible answers
type GameState = [[Card]]


-- a list of cards from Club R2 to Spade Ace
cardList = [Card Club R2 .. Card Spade Ace]


-- This function is used to built the initial GameState, take the arguments: cardList and the number of Cards
buildGameStates :: [a] -> Int -> [[a]]
buildGameStates _  0 = [[]]
buildGameStates [] _ =  []
buildGameStates (x:xs) k = withHead ++ withoutHead
  where 
    withHead    = map (x:) (buildGameStates xs (k-1))
    withoutHead = buildGameStates xs k


-- Init the guess, return a tuple which contains a guess and GameState
initialGuess :: Int -> ([Card],GameState)
initialGuess n = (guess, answerlist)
  where 
    answerlist = buildGameStates cardList n 
    guess = firstGuess n (head answerlist)


-- | The best first guess would be to choose two cards of different suits and with ranks
--   about equally distant from each other and from the top and bottom ranks.
--   This function, only when n < 5, chooses the first guess according to the above method.
--   When n > 5, it chooses the first n elements in GameState as the first guess.
firstGuess::Int -> [Card] -> [Card]
firstGuess n head_pos_answer
  | n == 1 = [Card Club R8]
  | n == 2 = [Card Club R6, Card Diamond R10]
  | n == 3 = [Card Club R5, Card Diamond R8, Card Heart Jack]
  | n == 4 = [Card Club R4, Card Diamond R7, Card Heart R10, Card Spade Queen]
  | n == 5 = [Card Club R3, Card Diamond R5, Card Heart R7, Card Spade R9, Card Club Jack]
  | otherwise = head_pos_answer 


-- | Give the next guess according to the previous guess, GameState and the feedback to the previous guess
--   remove the impossible answers from the GameState according to  previous guess and the feedback to the previous guess
--   there is 2 ways to choose the next guess:
--   1. when n ==1 or n ==2, choose the guess that leaves the smallest remaining list of possible answers
--   2. when n > 3, choose the first element from the remaining possible answers(GameState).
--   This is because method1 is time-consuming but can achieve a high quality while method2 is relatively faster
--   but often needs more steps of guess than method1
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (pre_guess,pre_answerlist) pre_feedback 
  | length pre_guess < 3  = (new_guess,new_answerlist)
  | otherwise = (head new_answerlist,new_answerlist)
  where 
    new_answerlist = filter(\x -> (feedback x pre_guess) == pre_feedback) pre_answerlist
    new_guess = (snd.minimum) (selectGuess new_answerlist new_answerlist )


-- | Take each remaining possible answer as the guess, then computing the feedback for each remaining possible answers,
--   and then group these answers by their feedback, finally calculate the he average of the sizes of these groups,
--   weighted by the sizes of the groups, returns a list of tuple: (average size of groups, guess). Function nextGuess chooses
--   the guess with smallest average size of groups as the next guess.
selectGuess::[[Card]] -> [[Card]] -> [(Double,[Card])]
selectGuess [] _ = []
selectGuess (x:xs) cardlist = (average_anwser,x):(selectGuess xs cardlist)
  where
    sizes_groups = map length ((group.sort) [feedback a x | a <- cardlist])
    average_anwser = fromIntegral (sum (map (^2) sizes_groups)) / fromIntegral (sum sizes_groups)







