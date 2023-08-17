-- convert an integer (series of digits) 
-- into list of individual digits
-- Ex: toDigits 1234 = [1, 2, 3, 4]
toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else 
    (toDigits (x `div` 10)) ++ [(x `mod` 10)]

-- convert a series of digits 
-- into list of individual digits,
-- but reversed.
-- Ex: toDigitsRev 1234 -> [4, 3, 2, 1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOtherSimple :: [Integer] -> [Integer]
doubleEveryOtherSimple [] = []
doubleEveryOtherSimple (x:xs) = [x] ++ [(head xs) * 2] ++ (doubleEveryOtherSimple (tail xs))

-- doubleEveryOther: doubles every other number in list
-- beginning from right.
-- Ex: [1, 2, 3] -> [1, 4, 3]
-- Ex: [8, 7, 6, 5] -> [5, 6, 7, 8] -> [5, 12, 7, 16] -> [16, 7, 12, 5]
-- Ex: rev 8765 -> [5, 6, 7, 8] -> double: 5, 12, 7, 16 -> rev: 16, 7, 12, 5
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther x = reverse (doubleEveryOtherSimple (reverse x))

-- find length of a number/number of digits
lengthOfNumber :: Integer -> Integer 
lengthOfNumber 0 = 0
lengthOfNumber x = 1 + (lengthOfNumber (x `div` 10))

-- sumDigits: given a list of ints, get the sum of the digits
-- of the ints.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (y:ys) = if (lengthOfNumber y) >= 2 
    then (sumDigits (toDigits y)) + (sumDigits ys)
    else y + (sumDigits ys)

-- given a potential credit card number, check if number is potentially valid
validate :: Integer -> Bool
validate x = if ((sumDigits (doubleEveryOther (toDigits x))) `mod` 10) == 0 
    then True 
    else False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = [("","")]
hanoi x p1 p2 p3 =