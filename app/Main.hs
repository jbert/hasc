module Main where

import Hasc.Eval
import Hasc.Lex
import Hasc.Parse

main :: IO ()
main = do
    -- let prog = "(+ \"foo\" 2)"
    -- let prog = "(+ \"foo\" \"bar\")"
    -- let prog = "(+ 1 2)"
    -- let prog = "(if #t (+ 1 2) (+ 3 4))"
    let prog = "(if #f 2 3)"
    let eexpr = do
            toks <- hLex prog
            e <- hParse toks
            return e
    print eexpr
    let env = mkDefaultEnv
    print env
    let res = (eval env) =<< eexpr
    print res
