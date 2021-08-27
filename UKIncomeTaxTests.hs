import Test.QuickCheck
import Test.HUnit

import UKIncomeTax

{- 1. Property Tests with QuickCheck -}

{- 1.1 Integer range for test data -}

newtype IntTest = IntTest {
    unIntTest :: Int
} deriving (Eq, Ord, Show)

instance Arbitrary IntTest where
    arbitrary = do 
        n <- choose ((-10000), 200000)
        return (IntTest n)

{- 1.1 personalAllowance -}

-- between 0 and 12500
prop_pa1 :: IntTest -> Bool
prop_pa1 inc = (0 <= pa && pa <= 12500) 
    where pa = personalAllowance (i)
          i = unIntTest inc

-- ==12500 until 100'000
prop_pa2 :: IntTest -> Bool
prop_pa2 inc = (not (i <= 100000) || pa == 12500) 
    where pa = personalAllowance (i)
          i = unIntTest inc

-- ==0 after 125'000
prop_pa3 :: IntTest -> Bool
prop_pa3 inc = (not (i >= 125000) || pa == 0) 
    where pa = personalAllowance (i)
          i = unIntTest inc

-- within 100'000 to 125'000, decrease by -1£ on each 2nd £
prop_pa4 :: IntTest -> Property
prop_pa4 inc =
    let i = unIntTest inc
    in
    (100000<=i && i<125000) ==> 
        (even i && personalAllowance i == personalAllowance (i+1))
        ||
        (odd i && personalAllowance i - 1 == personalAllowance (i+1))

qc_pa1 = quickCheck (withMaxSuccess 1000  prop_pa1)
qc_pa2 = quickCheck (withMaxSuccess 1000  prop_pa2)
qc_pa3 = quickCheck (withMaxSuccess 1000  prop_pa3)
qc_pa4 = quickCheck (withMaxSuccess 1000  prop_pa4)
