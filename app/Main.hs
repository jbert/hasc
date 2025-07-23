module Main where

import Hasc.Lex

main :: IO ()
main = do
    -- let prog = "(+ \"foo\" 2)"
    let prog = "(+ \"foo\" \"bar\")"
    let toks = hLex prog
    print toks
