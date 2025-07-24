module Main where

import Hasc.Lex
import Hasc.Parse

main :: IO ()
main = do
    -- let prog = "(+ \"foo\" 2)"
    let prog = "(+ \"foo\" \"bar\")"
    let expr = do
            toks <- hLex prog
            e <- hParse toks
            return e
    print $ expr
