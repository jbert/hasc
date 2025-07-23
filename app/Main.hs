module Main where

import Hasc

main :: IO ()
main = do
    -- let prog = "(+ \"foo\" 2)"
    let prog = "(+ \"foo\" \"bar\")"
    let toks = hLex prog
    print toks
