module HLex (hLexTests) where

import Hasc
import Test.HUnit

data TD = TD {prog :: String, expected :: Either String [Token]}

testData :: [TD]
testData =
    [ TD "(" (Right [Open])
    , TD ")" (Right [Close])
    , TD "()" (Right [Open, Close])
    , TD "\"\"" (Right [Str ""])
    , TD "\"a\"" (Right [Str "a"])
    , TD "\"abc-d_e_5\"" (Right [Str "abc-d_e_5"])
    , TD "a" (Right [Sym "a"])
    , TD "abc-d_e_5" (Right [Sym "abc-d_e_5"])
    , TD "abc def" (Right [Sym "abc", Sym "def"])
    , TD "\"abc\" \"def\"" (Right [Str "abc", Str "def"])
    , TD "5" (Right [Nmb 5])
    , TD "52" (Right [Nmb 52])
    , TD "0" (Right [Nmb 0])
    , TD "\"a\\a\"" (Left "Unknown backslash escape: 'a'")
    , TD "(foo 0)" (Right [Open, Sym "foo", Nmb 0, Close])
    ]

----------------

mkTestCase :: TD -> Test
mkTestCase td =
    let got = hLex (prog td)
    in TestCase (assertEqual "Test" (expected td) got)

hLexTests :: Test
hLexTests = TestList $ map mkTestCase testData
