module Hasc.Eval where

import Hasc.Env
import Hasc.Lex
import Hasc.Parse

eval :: Env -> Expr -> Either String Expr
eval env (Atom (Val (Sym s))) = envLookup env s
eval _ (Atom a) = Right $ Atom a
eval _ (Callable _) = Left "Can't evaluate callable directly"
eval env (HList (ef : eargs)) = do
    f <- eval env ef
    args <- sequence $ map (eval env) eargs
    res <- case f of
        Callable c -> Right $ c args
        _ -> Left "Non-callable in callable position"
    res
eval _ (HList []) = Left "Eval empty list"
