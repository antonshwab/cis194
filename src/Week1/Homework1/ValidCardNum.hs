module Week1.Homework1.ValidCardNum where

toDigits :: Integer -> [Integer]
toDigits n
  | n == 0 || n < 0 = []
  | otherwise = aux n []
    where
      aux num digits =
        case num `divMod` 10 of
          (0, d) -> d : digits
          (rest, d) -> aux rest (d : digits)


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits = reverse $ map op digits'
  where
    digits' = zip (reverse digits) [1..(length digits)]
    op (digit, index) = if even index then digit * 2 else digit

sumDigits :: [Integer] -> Integer
sumDigits digits = sum (concatMap toDigits digits)

validate :: Integer -> Bool
validate cardNumber = result `mod` 10 == 0
  where result = sumDigits $ doubleEveryOther $ toDigits cardNumber



