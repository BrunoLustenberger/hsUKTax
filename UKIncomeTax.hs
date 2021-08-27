module UKIncomeTax (
      personalAllowance
    , taxableIncome
    , incomeTax
    ) where

{- Note: stepwise between 100'000 and 125'000, i.e.
    income      personalAllowance
    100'000     12'500
    100'001     12'500 -- no change yet
    100'002     12'499 -- only now first change
    etc.
-}
personalAllowance :: Int -> Int
personalAllowance inc = decrease inc `min` 12500 `max` 0
    where decrease inc = 62500 - inc `div` 2

taxableIncome :: Int -> Int
taxableIncome inc = (inc - personalAllowance inc) `max` 0

{- Rounded income tax: e.g. a tax of £4532.68 is rounded down to £4532 -}
incomeTax :: Int -> Int
incomeTax inc = incomeTax' inc `div` 100

{- Exact income text in pennies instead of pounds -}
incomeTax' :: Int -> Int
incomeTax' inc = 
    if tinc <= 37500 then       20 * tinc
    else if tinc <= 150000 then 40 * tinc
    else                        45 * tinc
    where tinc = taxableIncome inc



