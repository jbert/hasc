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
    -- let prog = "(if #f 2 3)"
    -- let prog = "(())"
    -- let prog = "(do #t (+ 1 2) (+ 3 4))"
    -- let prog = "((lambda (a b) (+ a b)) 1 2)"
    -- let prog = "(let ((a 1) (b 2)) (+ a b))"
    -- let prog = "(let ((addOne (lambda (x) (+ 1 x))) (b 2)) (addOne b))"
    let prog = "(sin pi)"
    let eexpr = do
            toks <- hLex prog
            e <- hParse toks
            return e
    print eexpr
    let env = mkDefaultEnv
    print env
    let res = (eval env) =<< eexpr
    print res
