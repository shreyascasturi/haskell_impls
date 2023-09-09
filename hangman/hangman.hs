import Data.List
import Data.Maybe


-- let there be a tuple which represents state:
-- (input, word visualization, actual word, number of remaining chances)
-- We want a function "generateNewState" :: [Char] -> [Char] -> [Char] -> Int -> (empty str, updated vis, actual word, updated num of chances)

-- When I enter a character, I must:
-- 1. Determine if it is/isn't in the string.
-- 2a. If in string, then determine if user already found that char at that occurrence.
    -- 2a.1. If not already found, then update word visualization, keep amount of remaining chances same
    -- 2a.2 If already found, then do not change word vis, decrement remaining chances
-- 2b -- If not in string, then do not change word vis, decrement remaining chances.

-- I need a function to create a [Char] with "-----", with dashes
-- I need a function to determine index of char in the string.
-- Really, I need one function to determine if all occurrences of the input char have been found
    -- or if there is still something left.
    -- if there is something left, then invoke the indexOfChar string.

-- check if the char is in the string to begin with. This is the first
    -- function invoked in the char-checking logic
checkIfCharIsInString :: Char -> [Char] -> Bool
checkIfCharIsInString _ [] = False
checkIfCharIsInString x [y] = if x == y then True else False
checkIfCharIsInString x (y:ys) = if x == y then True else (checkIfCharIsInString x ys)

indexOfChar :: Char -> [Char]  -> Maybe Int
indexOfChar _ []  = Nothing
indexOfChar y [x] = if y == x then Just 0 else Nothing 
indexOfChar y (x:xs) = if y == x 
                       then Just 0 
                       else 
                        if (isNothing (indexOfChar y xs)) 
                        then Nothing 
                        else Just (1 + fromMaybe 0 (indexOfChar y xs))


-- for a given index, replace the character at that index with a dash 
replaceCharAtIndexWithChar :: Int -> Char -> [Char] -> [Char]
replaceCharAtIndexWithChar 0 _ [] = []
replaceCharAtIndexWithChar index replaceChar (y:ys) = if index == 0 
                                                      then [replaceChar] ++ ys 
                                                      else [y] ++ replaceCharAtIndexWithChar (index - 1) replaceChar ys

-- this will be the "visualization" array, starting with all dashes and revealing the word.
createCharOfDashes :: Int -> [Char]
createCharOfDashes 0 = []
createCharOfDashes x = ['-'] ++ (createCharOfDashes (x - 1))

-- tryGuess takes a tuple (the character to guess, secret/vis string, real string, number of remaining chances)
-- returns a tuple (updated vis string, real string, updated number of remaining chances)
tryGuess :: Char -> [Char] -> [Char] -> Int -> ([Char], [Char], Int)
tryGuess guessChar visString realString remainingChances = if (checkIfCharIsInString guessChar realString) 
                                                           then ((replaceCharAtIndexWithChar (fromJust (indexOfChar guessChar realString)) guessChar visString), (replaceCharAtIndexWithChar (fromJust (indexOfChar guessChar realString)) '-' realString), remainingChances)
                                                           else (visString, realString, (remainingChances - 1))

firstElemOfTuple :: (a, b, c) -> a
firstElemOfTuple (a, _, _) = a

secondElemOfTuple :: (a, b, c) -> b
secondElemOfTuple (_, b, _) = b

thirdElemOfTuple :: (a, b, c) -> c
thirdElemOfTuple (_, _, c) = c