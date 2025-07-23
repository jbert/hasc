module Main where

import HLex
import Test.HUnit

tests :: Test
tests =
    TestList
        [ TestLabel "hLex" hLexTests
        ]

main :: IO ()
main = runTestTTAndExit tests
