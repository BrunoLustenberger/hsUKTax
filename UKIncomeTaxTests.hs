import Test.QuickCheck
import Test.HUnit

import UKIncomeTax

{- 1. Property Tests with QuickCheck -}

-- personalAllowance

-- between 0 and 12500
prop_pa1 :: Int -> Bool
prop_pa1 inc = (0 <= pa && pa <= 12500) 
    where pa = personalAllowance inc

-- ==12500 until 100'000
prop_pa2 :: Int -> Bool
prop_pa2 inc = (not (inc <= 100000) || pa == 12500) 
    where pa = personalAllowance inc

-- ==0 after 125'000
prop_pa3 :: Int -> Bool
prop_pa3 inc = (not (inc >= 125000) || pa == 0) 
    where pa = personalAllowance inc

-- within 100'000 to 125'000, decrease by -1£ on each 2nd £
prop_pa4 :: Int -> Property
prop_pa4 inc =
    (100000<=inc && inc<125000) ==> 
        (even inc && personalAllowance inc == personalAllowance (inc+1))
        ||
        (odd inc && personalAllowance inc - 1 == personalAllowance (inc+1))

qc_pa1 = quickCheck (withMaxSuccess 1000  prop_pa1)
qc_pa2 = quickCheck (withMaxSuccess 1000  prop_pa2)
qc_pa3 = quickCheck (withMaxSuccess 1000  prop_pa3)
qc_pa4 = quickCheck (withMaxSuccess 1000  prop_pa4)
