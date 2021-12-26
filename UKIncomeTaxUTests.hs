{- |
Module : UKIncomeTaxUTests
Description : unit testcases for module UKIncomeTax
Copyright : (c) 2021 Bruno M.S. Lustenberger

The following testcases are double checked with the excel table samples.xlsx. 

You can run these unit testcases from ghci using the command runTestTT.
Here is a sample session: first the module is loaded, then a single
testcase pa7 is executed, then all testcases for personalAllowance
and finally all testcases.

$> ghci
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /Users/Ls/.ghc/x86_64-darwin-8.10.4/environments/default
Prelude> :l UKIncomeTaxUTests.hs
[1 of 2] Compiling UKIncomeTax      ( UKIncomeTax.hs, interpreted )
[2 of 2] Compiling UKIncomeTaxUTests ( UKIncomeTaxUTests.hs, interpreted )
Ok, two modules loaded.
*UKIncomeTaxUTests> runTestTT pa7
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
*UKIncomeTaxUTests> runTestTT paTests
Cases: 13  Tried: 13  Errors: 0  Failures: 0
Counts {cases = 13, tried = 13, errors = 0, failures = 0}
*UKIncomeTaxUTests> runTestTT allTestCases
Cases: 72  Tried: 72  Errors: 0  Failures: 0
Counts {cases = 72, tried = 72, errors = 0, failures = 0}
*UKIncomeTaxUTests> 
-}
module UKIncomeTaxUTests where

import Test.HUnit

import UKIncomeTax

{- 1 personalAllowance -}

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

-- |Testcases for personalAllowance
paTests = TestLabel "pa" 
    (TestList [ pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8,pa8',pa9,pa10,
                pa11,pa12,pa13])

{- 2 taxableIncome -}

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

-- |Testcases for taxableIncome
tiTests = TestLabel "ti" 
    (TestList [ ti1,ti2,ti3,ti4,ti5,ti6,ti7,ti8,ti9,ti10,
                ti11,ti12,ti13,ti14,ti15,ti16,ti17])

{- 3 incomeTax -}

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

-- |Testcases for incomeTax
itTests = TestLabel "it" 
    (TestList [ it0, 
                it1,it2,it3,it4,it5,it6,it7,it8,it9,it10,
                it11,it12,it13,it14,it15,it16,
                it20,it21,it22,it23,it24,it25,it26,it27,it28,
                it30,it31,it32,it33,it34,it35,it36,
                it40,it41,it42,it43,it44,it45,it46,
                it50,it51])



{- 4 all Tests -}

-- |All testcases 
allTestCases = TestList [paTests, tiTests, itTests]
