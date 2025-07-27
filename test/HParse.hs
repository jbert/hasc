module HParse where

import Data.Either

import Hasc.Eval
import Hasc.Lex
import Hasc.Parse
import Test.HUnit

data TD = TD {prog :: String, expected :: Either String Expr}

testData :: [TD]
testData =
    [ TD "1" (Right (Atom $ Val $ Nbr 1.0))
    {-
    , TD
        "(if #t (+ 1 2) (+ 3 4))"
        ( Right
            [ Open
            , Val $ Sym "if"
            , Val $ Bol True
            , Open
            , Val $ Sym "+"
            , Val $ Nbr 1.0
            , Val $ Nbr 2.0
            , Close
            , Open
            , Val $ Sym "+"
            , Val $ Nbr 3.0
            , Val $ Nbr 4.0
            , Close
            , Close
            ]
        )
        -}
    ]

----------------

mkTestCase :: TD -> Test
mkTestCase td =
    -- Fail to lex is a an error writing the test
    let toks = fromRight [Val $ Str "Bad test"] $ hLex (prog td)
        got = hParse toks
    in TestCase (assertEqual "Test" (expected td) got)

hParseTests :: Test
hParseTests = TestList $ map mkTestCase testData
