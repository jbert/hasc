module Hasc.Parse where

import Hasc.Lex

data Expr = Atom Token | HList [Expr]

instance Show Expr where
    show (Atom t) = show t
    show (HList es) = "(" ++ (unwords $ map show es) ++ ")"

hParseList :: [Expr] -> [Token] -> Either String (Expr, [Token])
hParseList _ [] = Left "No closing parens for list expression"
hParseList acc (Close : rest) = Right (HList $ reverse acc, rest)
hParseList acc (tok : rest) = do
    (expr, _) <- hParse [tok]
    hParseList (expr : acc) rest

hParse :: [Token] -> Either String (Expr, [Token])
hParse [] = Left "Empty expression"
hParse (atom@(Nmb _) : rest) = Right (Atom atom, rest)
hParse (atom@(Sym _) : rest) = Right (Atom atom, rest)
hParse (atom@(Str _) : rest) = Right (Atom atom, rest)
hParse (Open : rest) = hParseList [] rest
hParse (_ : _) = Left "Need open parens at start of list"
