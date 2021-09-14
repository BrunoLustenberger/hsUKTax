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

--pendenz: income == pa + summe er 3 chunks

{- 2. Unit Tests with HUnit -}

{- 2.1 personalAllowance -}

pa1  = TestCase $ assertEqual "pa1"     (personalAllowance (-1))        12500
pa2  = TestCase $ assertEqual "pa2"     (personalAllowance 0)           12500
pa3  = TestCase $ assertEqual "pa3"     (personalAllowance 1)           12500
pa4  = TestCase $ assertEqual "pa4"     (personalAllowance 99999)       12500
pa5  = TestCase $ assertEqual "pa5"     (personalAllowance 100000)      12500
pa6  = TestCase $ assertEqual "pa6"     (personalAllowance 100001)      12500
pa7  = TestCase $ assertEqual "pa7"     (personalAllowance 100002)      12499
pa8  = TestCase $ assertEqual "pa8"     (personalAllowance 100003)      12499
pa8' = TestCase $ assertEqual "pa8'"    (personalAllowance 100004)      12498
pa9  = TestCase $ assertEqual "pa9"     (personalAllowance 124997)      2
pa10 = TestCase $ assertEqual "pa10"    (personalAllowance 124998)      1
pa11 = TestCase $ assertEqual "pa11"    (personalAllowance 124999)      1
pa12 = TestCase $ assertEqual "pa12"    (personalAllowance 125000)      0
pa13 = TestCase $ assertEqual "pa13"    (personalAllowance 125001)      0

paTests = TestLabel "pa" (TestList [pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8,pa9,pa10,
                                    pa11,pa12,pa13])

{- 2.2 taxableIncome -}

ti1   = TestCase $ assertEqual "ti1"   (taxableIncome (-1))      0
ti2   = TestCase $ assertEqual "ti2"   (taxableIncome 0)         0
ti3   = TestCase $ assertEqual "ti3"   (taxableIncome 1)         0
ti4   = TestCase $ assertEqual "ti4"   (taxableIncome 12499)     0
ti5   = TestCase $ assertEqual "ti5"   (taxableIncome 12500)     0
ti6   = TestCase $ assertEqual "ti6"   (taxableIncome 12501)     1
ti7   = TestCase $ assertEqual "ti7"   (taxableIncome 99999)    87499
ti8   = TestCase $ assertEqual "ti8"   (taxableIncome 100000)   87500
ti9   = TestCase $ assertEqual "ti9"   (taxableIncome 100001)   87501
ti10  = TestCase $ assertEqual "ti10"  (taxableIncome 100002)   87503
ti11  = TestCase $ assertEqual "ti11"  (taxableIncome 100003)   87504
ti12  = TestCase $ assertEqual "ti12"  (taxableIncome 100004)   87506
ti13  = TestCase $ assertEqual "ti13"  (taxableIncome 100005)   87507
ti14  = TestCase $ assertEqual "ti14"  (taxableIncome 124998)   124997
ti15  = TestCase $ assertEqual "ti15"  (taxableIncome 124999)   124998
ti16  = TestCase $ assertEqual "ti16"  (taxableIncome 125000)   125000
ti17  = TestCase $ assertEqual "ti17"  (taxableIncome 125001)   125001

tiTests = TestLabel "ti" (TestList [ti1,ti2,ti3,ti4,ti5,ti6,ti7,ti8,ti9,ti10,
                                    ti11,ti12,ti13,ti14,ti15,ti16,ti17])

{- 2.3 incomeTax -}

it0  =  TestCase $ assertEqual "it0"   (incomeTax 72500)     16500

it1   = TestCase $ assertEqual "it1"   (incomeTax (-1))      0
it2   = TestCase $ assertEqual "it2"   (incomeTax 0)         0
it3   = TestCase $ assertEqual "it3"   (incomeTax 1)         0
it4   = TestCase $ assertEqual "it4"   (incomeTax 12499)     0
it5   = TestCase $ assertEqual "it5"   (incomeTax 12500)     0
it6   = TestCase $ assertEqual "it6"   (incomeTax 12501)     0 -- 0.2
it7   = TestCase $ assertEqual "it7"   (incomeTax 12502)     0 -- 0.4
it8   = TestCase $ assertEqual "it8"   (incomeTax 12503)     0 -- 0.6
it9   = TestCase $ assertEqual "it9"   (incomeTax 12504)     0 -- 0.8
it10  = TestCase $ assertEqual "it10"  (incomeTax 12505)     1 -- 1.0

it11  = TestCase $ assertEqual "it11"  (incomeTax 49998)     7499 -- 7499.60
it12  = TestCase $ assertEqual "it12"  (incomeTax 49999)     7499 -- 7499.80
it13  = TestCase $ assertEqual "it13"  (incomeTax 50000)     7500 -- 7500.00
it14  = TestCase $ assertEqual "it14"  (incomeTax 50001)     7500 -- 7500.40
it15  = TestCase $ assertEqual "it15"  (incomeTax 50002)     7500 -- 7500.80
it16  = TestCase $ assertEqual "it16"  (incomeTax 50003)     7501 -- 7501.20

it20  = TestCase $ assertEqual "it20"  (incomeTax 99998)     27499 -- 27499.20
it21  = TestCase $ assertEqual "it21"  (incomeTax 99999)     27499 -- 27499.60
it22  = TestCase $ assertEqual "it22"  (incomeTax 100000)    27500 -- 27500.00
it23  = TestCase $ assertEqual "it23"  (incomeTax 100001)    27500 -- 27500.40
it24  = TestCase $ assertEqual "it24"  (incomeTax 100002)    27501 -- 27501.20
it25  = TestCase $ assertEqual "it25"  (incomeTax 100003)    27501 -- 27501.60
it26  = TestCase $ assertEqual "it26"  (incomeTax 100004)    27502 -- 27502.40
it27  = TestCase $ assertEqual "it27"  (incomeTax 100005)    27502 -- 27502.80
it28  = TestCase $ assertEqual "it28"  (incomeTax 100006)    27503 -- 27503.60

it30  = TestCase $ assertEqual "it30"  (incomeTax 124997)    42498 -- 42498.00
it31  = TestCase $ assertEqual "it31"  (incomeTax 124998)    42498 -- 42498.80
it32  = TestCase $ assertEqual "it32"  (incomeTax 124999)    42499 -- 42499.20
it33  = TestCase $ assertEqual "it33"  (incomeTax 125000)    42500 -- 42500.00
it34  = TestCase $ assertEqual "it34"  (incomeTax 125001)    42500 -- 42500.40
it35  = TestCase $ assertEqual "it35"  (incomeTax 125002)    42500 -- 42500.80
it36  = TestCase $ assertEqual "it36"  (incomeTax 125003)    42501 -- 42501.20

it40  = TestCase $ assertEqual "it40"  (incomeTax 149997)    52498 -- 52498.80
it41  = TestCase $ assertEqual "it41"  (incomeTax 149998)    52499 -- 52499.20
it42  = TestCase $ assertEqual "it42"  (incomeTax 149999)    52499 -- 52499.60
it43  = TestCase $ assertEqual "it43"  (incomeTax 150000)    52500 -- 52500.00
it44  = TestCase $ assertEqual "it44"  (incomeTax 150001)    52500 -- 52500.45
it45  = TestCase $ assertEqual "it45"  (incomeTax 150002)    52500 -- 52500.90
it46  = TestCase $ assertEqual "it46"  (incomeTax 150003)    52501 -- 52501.35

it50  = TestCase $ assertEqual "it50"  (incomeTax 150010)    52504 -- 52504.50
it51  = TestCase $ assertEqual "it51"  (incomeTax 160000)    57000 -- 57000.00


itTests = TestLabel "it" (TestList [it0, 
                                    it1,it2,it3,it4,it5,it6,it7,it8,it9,it10,
                                    it11,it12,it13,it14,it15,it16,
                                    it20,it21,it22,it23,it24,it25,it26,it27,it28,
                                    it30,it31,it32,it33,it34,it35,it36,
                                    it40,it41,it42,it43,it44,it45,it46,
                                    it50,it51])



{- 2.4 all Tests -}

allTestCases = TestList [paTests, tiTests, itTests]