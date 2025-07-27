module HParse where

import Data.Either

import Hasc.Eval
import Hasc.Lex
import Hasc.Parse
import Test.HUnit

data TD = TD {prog :: String, expected :: Either String Expr}

testData :: [TD]
testData =
    [ TD "1" (Right (Atom $ Nbr 1.0))
    , TD
        "(+ 1 2)"
        ( Right
            ( HList
                [ Atom $ Sym "+"
                , Atom $ Nbr 1.0
                , Atom $ Nbr 2.0
                ]
            )
        )
    , TD "()" (Right (HList []))
    , TD "(())" (Right (HList [HList []]))
    , TD
        "(if #t (+ 1 2) (+ 3 4))"
        ( Right
            ( HList
                [ Atom $ Sym "if"
                , Atom $ Bol True
                , HList
                    [ Atom $ Sym "+"
                    , Atom $ Nbr 1.0
                    , Atom $ Nbr 2.0
                    ]
                , HList
                    [ Atom $ Sym "+"
                    , Atom $ Nbr 3.0
                    , Atom $ Nbr 4.0
                    ]
                ]
            )
        )
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
