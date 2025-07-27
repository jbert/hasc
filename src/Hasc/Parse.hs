module Hasc.Parse where

import Hasc.Eval
import Hasc.Lex

hParseList :: [Expr] -> [Token] -> Either String (Expr, [Token])
hParseList _ [] = Left "No closing parens for list expression"
hParseList acc (Close : rest) = Right (HList $ reverse acc, rest)
hParseList acc (tok : rest) = do
    (expr, _) <- hParseOne [tok]
    hParseList (expr : acc) rest

hParseOne :: [Token] -> Either String (Expr, [Token])
hParseOne [] = Left "Empty expression"
hParseOne (atom@(Val _) : rest) = Right (Atom atom, rest)
hParseOne (Open : rest) = hParseList [] rest
hParseOne (_ : _) = Left "Need open parens at start of list"

hParse :: [Token] -> Either String Expr
hParse toks = do
    (expr, rest) <- hParseOne toks
    if length rest > 0
        then
            Left "Multiple expressions"
        else
            Right expr
