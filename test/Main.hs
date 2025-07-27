module Main where

import HLex
import HParse
import Test.HUnit

tests :: Test
tests =
    TestList
        [ TestLabel "hLex" hLexTests
        , TestLabel "hParse" hParseTests
        ]

main :: IO ()
main = runTestTTAndExit tests
