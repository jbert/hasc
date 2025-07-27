module HLex (hLexTests) where

import Hasc.Lex
import Test.HUnit

data TD = TD {prog :: String, expected :: Either String [Token]}

testData :: [TD]
testData =
    [ TD "(" (Right [Open])
    , TD ")" (Right [Close])
    , TD "))" (Right [Close, Close])
    , TD "()" (Right [Open, Close])
    , TD ")(" (Right [Close, Open])
    , TD "(())" (Right [Open, Open, Close, Close])
    , TD "((a))" (Right [Open, Open, Val $ Sym "a", Close, Close])
    , TD "\"\"" (Right [Val $ Str ""])
    , TD "\"a\"" (Right [Val $ Str "a"])
    , TD "\"abc-d_e_5\"" (Right [Val $ Str "abc-d_e_5"])
    , TD "a" (Right [Val $ Sym "a"])
    , TD "abc-d_e_5" (Right [Val $ Sym "abc-d_e_5"])
    , TD "abc def" (Right [Val $ Sym "abc", Val $ Sym "def"])
    , TD "\"abc\" \"def\"" (Right [Val $ Str "abc", Val $ Str "def"])
    , TD "5" (Right [Val $ Nbr 5])
    , TD "52" (Right [Val $ Nbr 52])
    , TD "0" (Right [Val $ Nbr 0])
    , TD "\"a\\a\"" (Left "Unknown backslash escape: 'a'")
    , TD "(foo 0)" (Right [Open, Val $ Sym "foo", Val $ Nbr 0, Close])
    , TD "#t" (Right [Val $ Bol True])
    , TD "#f" (Right [Val $ Bol False])
    , TD "#a" (Left "Non-bool token: #a")
    , TD "(a (b))" (Right [Open, Val $ Sym "a", Open, Val $ Sym "b", Close, Close])
    ]

----------------

mkTestCase :: TD -> Test
mkTestCase td =
    let got = hLex (prog td)
    in TestCase (assertEqual "Test" (expected td) got)

hLexTests :: Test
hLexTests = TestList $ map mkTestCase testData
