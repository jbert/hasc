module Hasc.Lex
where

import Data.Char

-- import Debug.Trace

data Val
    = Sym {symVal :: String}
    | Nbr {nbrVal :: Double}
    | Str {strVal :: String}
    | Bol {boolVal :: Bool}
    deriving (Eq)

data Token
    = Open
    | Close
    | Val Val
    deriving (Eq)

instance Show Token where
    show Open = "("
    show Close = ")"
    show (Val (Sym s)) = s
    show (Val (Str s)) = s
    show (Val (Nbr n)) = show n
    show (Val (Bol v)) = show v

insertToken :: Token -> String -> Either String [Token]
insertToken t s = fmap (t :) (hLex s)

hLexInStr :: String -> String -> Either String [Token]
hLexInStr s ('"' : rest) = insertToken (Val $ Str s) rest
hLexInStr _ ('\\' : "") = Left "Backslash in string with no closing quote"
hLexInStr s ('\\' : '\\' : rest) = hLexInStr (s ++ "\\") rest
hLexInStr s ('\\' : '"' : rest) = hLexInStr (s ++ "\"") rest
hLexInStr _ ('\\' : c : _) = Left ("Unknown backslash escape: '" ++ [c] ++ "'")
hLexInStr s (c : rest) = hLexInStr (s ++ [c]) rest
hLexInStr _ [] = Left "String with no closing quote"

hLexBool :: String -> Either String [Token]
hLexBool ('t' : rest) = insertToken (Val $ Bol True) rest
hLexBool ('f' : rest) = insertToken (Val $ Bol False) rest
hLexBool (c : _) = Left $ "Non-bool token: #" ++ [c]
hLexBool [] = Left "# with no t or f"

alphaChars :: String
alphaChars = ['a' .. 'z'] ++ ['A' .. 'Z']

digitChars :: String
digitChars = ['0' .. '9']

symChars :: String
symChars = alphaChars ++ digitChars ++ ['_', '-', '+']

hLexInSym :: String -> String -> Either String [Token]
hLexInSym s [] = Right $ [Val $ Sym s]
hLexInSym s (c : rest)
    | c `elem` symChars = hLexInSym (s ++ [c]) rest
    | otherwise = insertToken (Val $ Sym s) (c : rest)

hLexNum :: Double -> String -> Either String [Token]
hLexNum n [] = Right $ [Val $ Nbr n]
hLexNum n s@(c : rest)
    | c `elem` digitChars = hLexNum ((n * 10) + (fromIntegral $ digitToInt c)) rest
    | otherwise = insertToken (Val $ Nbr n) s

hLex :: String -> Either String [Token]
-- hLex s | trace (show s) False = undefined
hLex [] = Right []
hLex s@(c : rest)
    | c == '(' = insertToken Open rest
    | c == ')' = insertToken Close rest
    | c == ' ' = hLex rest
    | c == '"' = hLexInStr "" rest
    | c == '#' = hLexBool rest
    | c `elem` digitChars = hLexNum 0 s
    | otherwise = hLexInSym "" s
