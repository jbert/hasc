module Hasc.Eval where

import Hasc.Env
import Hasc.Lex
import Hasc.Parse

eval :: Env -> Expr -> Either String Expr
eval env (Atom (Val (Sym s))) = envLookup env s
eval _ (Atom a) = Right $ Atom a
eval env (HList (ef : eargs)) = do
    f <- eval env ef
    args <- sequence $ map (eval env) eargs
    return f
eval _ (HList []) = Left "Eval empty list"
