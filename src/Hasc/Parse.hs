module Hasc.Parse where

import Hasc.Lex

data Expr = Atom Token | HList [Expr] | Callable ([Expr] -> Either String Expr)

instance Show Expr where
    show (Atom t) = show t
    show (HList es) = "(" ++ (unwords $ map show es) ++ ")"
    show (Callable _) = "<callable>"

asNum :: Expr -> Maybe Double
asNum (Atom (Val (Nbr x))) = Just x
asNum _ = Nothing

asString :: Expr -> Maybe String
asString (Atom (Val (Str x))) = Just x
asString (Atom (Val (Sym x))) = Just x
asString _ = Nothing

hParseList :: [Expr] -> [Token] -> Either String (Expr, [Token])
hParseList _ [] = Left "No closing parens for list expression"
hParseList acc (Close : rest) = Right (HList $ reverse acc, rest)
hParseList acc (tok : rest) = do
    (expr, _) <- hParseOne [tok]
    hParseList (expr : acc) rest

hParseOne :: [Token] -> Either String (Expr, [Token])
hParseOne [] = Left "Empty expression"
hParseOne (atom@(Val (Nbr _)) : rest) = Right (Atom atom, rest)
hParseOne (atom@(Val (Sym _)) : rest) = Right (Atom atom, rest)
hParseOne (atom@(Val (Str _)) : rest) = Right (Atom atom, rest)
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
