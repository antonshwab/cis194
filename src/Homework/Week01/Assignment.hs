module Homework.Week01.Assignment where

-- #1 - #4
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = let (rest, digit) = x `divMod` 10
                   in digit : toDigitsRev rest

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith double [1..]
  where double index digit = if even index then digit*2 else digit

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate cardNumber = result `mod` 10 == 0
  where result = sumDigits $ doubleEveryOther $ toDigitsRev cardNumber

-- #5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi disk source dest spare =
  hanoi (disk-1) source spare dest ++ [(source,dest)] ++ hanoi (disk-1) spare dest source

