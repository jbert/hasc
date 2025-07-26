module Main where

import Hasc.Env
import Hasc.Eval
import Hasc.Lex
import Hasc.Parse

main :: IO ()
main = do
    -- let prog = "(+ \"foo\" 2)"
    let prog = "(+ \"foo\" \"bar\")"
    let eexpr = do
            toks <- hLex prog
            e <- hParse toks
            return e
    print eexpr
    let env = mkDefaultEnv
    print env
    let res = fmap (eval env) eexpr
    print res
