module Main where

import HLex
import Test.HUnit

tests =
    TestList
        [ TestLabel "hLex" hLexTests
        ]

main = runTestTTAndExit tests
