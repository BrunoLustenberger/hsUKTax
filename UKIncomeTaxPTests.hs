{- |
Module : UKIncomeTaxPTests
Description : Property tests for module UKIncomeTax
Copyright : (c) Bruno M.S. Lustenberger

You can run these property tests from ghci by calling the functions
quickCheck or verboseCheck. Examples:

$> ghci
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /Users/Ls/.ghc/x86_64-darwin-8.10.4/environments/default
Prelude> :l UKIncomeTaxPTests.hs
[1 of 2] Compiling UKIncomeTax      ( UKIncomeTax.hs, interpreted )
[2 of 2] Compiling UKIncomeTaxPTests ( UKIncomeTaxPTests.hs, interpreted )
Ok, two modules loaded.
*UKIncomeTaxPTests> 
-- quickCheck, 100 tests
*UKIncomeTaxPTests> quickCheck propPa4
+++ OK, passed 100 tests; 741 discarded.
-- quickCheck, 1000 tests
*UKIncomeTaxPTests> quickCheck $ withMaxSuccess 1000  propPa4
+++ OK, passed 1000 tests; 7856 discarded.
-- verboseCheck, 100 tests
*UKIncomeTaxPTests> verboseCheck propPa4
...lot of output...
+++ OK, passed 100 tests; 813 discarded.
-- verboseCheck, 1000 tests
*UKIncomeTaxPTests> verboseCheck $ withMaxSuccess 1000  propPa4
...lot of output...
+++ OK, passed 1000 tests; 7102 discarded.
-}
module UKIncomeTaxPTests (
      propPa1, propPa2, propPa3, propPa4
    , propTi1, propTi2
    , propIt1, propIt2
    ) where

import Test.QuickCheck

import UKIncomeTax


{- 0 Integer range for test data.
     Necessary to concentrate the tests on a relevant range of integers.
-}

newtype IntTest = IntTest {
    unIntTest :: Int
} deriving (Eq, Ord, Show)

instance Arbitrary IntTest where
    arbitrary = do 
        n <- choose ((-10000), 200000)
        return (IntTest n)


{- 1 personalAllowance -}

-- |Between 0 and 12500
propPa1 :: IntTest -> Bool
propPa1 incTest = (0 <= pa && pa <= 12500) 
    where pa = personalAllowance (inc)
          inc = unIntTest incTest

-- | == 12500 until 100'000
propPa2 :: IntTest -> Bool
propPa2 incTest = (not (inc <= 100000) || pa == 12500) 
    where pa = personalAllowance (inc)
          inc = unIntTest incTest

-- | == 0 after 125'000
propPa3 :: IntTest -> Bool
propPa3 incTest = (not (inc >= 125000) || pa == 0) 
    where pa = personalAllowance (inc)
          inc = unIntTest incTest

-- |Within 100'000 to 125'000, decrease by -1£ on each 2nd £
propPa4 :: IntTest -> Property
propPa4 incTest =
    let inc = unIntTest incTest
    in
    (100000<=inc && inc<125000) ==> 
        (even inc && personalAllowance inc == personalAllowance (inc+1))
        ||
        (odd inc && personalAllowance inc - 1 == personalAllowance (inc+1))

-- todo: income == personalAllowance + sum of the 3 chunks


{- 2 taxable income -}

-- | == 0 if income low enough
propTi1 :: IntTest -> Property
propTi1 incTest =
    (inc <= 12500) ==> taxableIncome inc == 0
    where inc = unIntTest incTest

-- | == income if income high enough
propTi2 :: IntTest -> Property
propTi2 incTest =
    (inc >= 125000) ==> taxableIncome inc == inc
    where inc = unIntTest incTest


{- 3 income tax -}

-- |Smaller income, smaller tax
propIt1 :: IntTest -> IntTest -> Bool
propIt1 incTest1 incTest2 =
    if inc1 <= inc2 then tax1 <= tax2
    else                 tax1 >= tax2
    where inc1 = unIntTest incTest1
          inc2 = unIntTest incTest2
          tax1 = incomeTax inc1
          tax2 = incomeTax inc2

-- |Tax <= taxable income
propIt2 :: IntTest -> Bool
propIt2 incTest =
    incomeTax inc <= taxableIncome inc
    where inc = unIntTest incTest
