module HLex (hLexTests) where

import Hasc
import Test.HUnit

data TD = TD {prog :: String, expected :: Either String [Token]}

testData :: [TD]
testData =
    [ TD "(" (Right [Open])
    , TD ")" (Right [Close])
    , TD "()" (Right [Open, Close])
    ]

----------------

mkTestCase :: TD -> Test
mkTestCase td =
    let got = hLex (prog td)
    in TestCase (assertEqual "Test" (expected td) got)

hLexTests :: Test
hLexTests = TestList $ map mkTestCase testData
