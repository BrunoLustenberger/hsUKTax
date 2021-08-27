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

{- 1.2 personalAllowance -}

-- between 0 and 12500
prop_pa1 :: IntTest -> Bool
prop_pa1 incTest = (0 <= pa && pa <= 12500) 
    where pa = personalAllowance (inc)
          inc = unIntTest incTest

-- ==12500 until 100'000
prop_pa2 :: IntTest -> Bool
prop_pa2 incTest = (not (inc <= 100000) || pa == 12500) 
    where pa = personalAllowance (inc)
          inc = unIntTest incTest

-- ==0 after 125'000
prop_pa3 :: IntTest -> Bool
prop_pa3 incTest = (not (inc >= 125000) || pa == 0) 
    where pa = personalAllowance (inc)
          inc = unIntTest incTest

-- within 100'000 to 125'000, decrease by -1£ on each 2nd £
prop_pa4 :: IntTest -> Property
prop_pa4 incTest =
    let inc = unIntTest incTest
    in
    (100000<=inc && inc<125000) ==> 
        (even inc && personalAllowance inc == personalAllowance (inc+1))
        ||
        (odd inc && personalAllowance inc - 1 == personalAllowance (inc+1))

qc_pa1 = quickCheck (withMaxSuccess 1000  prop_pa1)
qc_pa2 = quickCheck (withMaxSuccess 1000  prop_pa2)
qc_pa3 = quickCheck (withMaxSuccess 1000  prop_pa3)
qc_pa4 = quickCheck (withMaxSuccess 1000  prop_pa4)

{- 1.3 taxable income -}

-- ==0 if income lowest
prop_ti1 :: IntTest -> Property
prop_ti1 incTest =
    (inc <= 12500) ==> taxableIncome inc == 0
    where inc = unIntTest incTest

-- == income if income hight
prop_ti2 :: IntTest -> Property
prop_ti2 incTest =
    (inc >= 125000) ==> taxableIncome inc == inc
    where inc = unIntTest incTest

qc_ti1 = quickCheck (withMaxSuccess 1000  prop_ti1)
qc_ti2 = quickCheck (withMaxSuccess 1000  prop_ti2)

{- 1.4 income tax -}

-- smaller income, smaller tax
prop_it1 :: IntTest -> IntTest -> Bool
prop_it1 incTest1 incTest2 =
    if inc1 <= inc2 then tax1 <= tax2
    else                 tax1 >= tax2
    where inc1 = unIntTest incTest1
          inc2 = unIntTest incTest2
          tax1 = incomeTax inc1
          tax2 = incomeTax inc2

-- tax <= taxable income
prop_it2 :: IntTest -> Bool
prop_it2 incTest =
    incomeTax inc <= taxableIncome inc
    where inc = unIntTest incTest

qc_it1 = quickCheck (withMaxSuccess 1000  prop_it1)
qc_it2 = quickCheck (withMaxSuccess 1000  prop_it2)

