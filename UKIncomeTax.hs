{- |
Module : UKIncomeTax
Description : Compute an individual's income tax from their annual income.
Copyright : (c) 2021 Bruno M.S. Lustenberger
Details see problemSpecs.md
-}
module UKIncomeTax (
      personalAllowance
    , taxableIncome
    , incomeTax
    ) where

{- |Compute personal allowance from annual income.
    Note: stepwise between 100'000 and 125'000, i.e.
    income      personalAllowance
    100'000     12'500
    100'001     12'500 -- no change yet
    100'002     12'499 -- only now first change
    etc.
-}
personalAllowance :: Int -> Int
personalAllowance inc = decrease inc `min` 12500 `max` 0
    where decrease inc = 62500 - inc `div` 2

-- |Compute taxable income from annual income.
taxableIncome :: Int -> Int
taxableIncome inc = (inc - personalAllowance inc) `max` 0
 
-- |Compute income tax from annual income.
--  Note: rounded, e.g. a tax of £4532.68 is rounded down to £4532
incomeTax :: Int -> Int
incomeTax inc = incomeTax100 inc `div` 100

-- |Exact income text in pennies instead of pounds
incomeTax100 :: Int -> Int
incomeTax100 inc = 20*tinc1 + 40*tinc2 + 45*tinc3 
--  20 instead of 0.20 --> factor 100
    where   tinc = taxableIncome inc
            tinc1 = tinc `min` 37500
            tinc2 = ((tinc `min` 150000) - 37500) `max` 0
            tinc3 = (tinc - 150000) `max` 0
