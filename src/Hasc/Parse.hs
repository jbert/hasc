module Hasc.Parse where

import Hasc.Eval
import Hasc.Lex

-- import Debug.Trace

hParseList :: [Expr] -> [Token] -> Either String (Expr, [Token])
-- hParseList es ts | trace (show es ++ "|" ++ show ts) False = undefined
hParseList acc (Close : rest) = Right (HList $ reverse acc, rest)
hParseList _ [] = Left "No closing parens for list expression"
hParseList acc toks = do
    -- traceM ("JB1: " ++ show tok ++ "|" ++ show rest)
    (expr, rest) <- hParseOne toks
    -- traceM ("JB2: " ++ show expr ++ "|" ++ show addrest)
    hParseList (expr : acc) rest

hParseOne :: [Token] -> Either String (Expr, [Token])
-- hParseOne s | trace ("JB3: " ++ (show s)) False = undefined
hParseOne [] = Left "Empty expression"
hParseOne ((Val v) : rest) = Right (Atom v, rest)
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
