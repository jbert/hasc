module Hasc.Lex
where

import Data.Char

-- import Debug.Trace

data Token
    = Open
    | Close
    | Sym {symVal :: String}
    | Nmb {nmbVal :: Double}
    | Str {strVal :: String}
    deriving (Eq)

instance Show Token where
    show Open = "("
    show Close = ")"
    show (Sym s) = s
    show (Str s) = s
    show (Nmb n) = show n

insertToken :: Token -> String -> Either String [Token]
insertToken t s = fmap (t :) (hLex s)

hLexInStr :: String -> String -> Either String [Token]
hLexInStr s ('"' : rest) = insertToken (Str s) rest
hLexInStr _ ('\\' : "") = Left "Backslash in string with no closing quote"
hLexInStr s ('\\' : '\\' : rest) = hLexInStr (s ++ "\\") rest
hLexInStr s ('\\' : '"' : rest) = hLexInStr (s ++ "\"") rest
hLexInStr _ ('\\' : c : _) = Left ("Unknown backslash escape: '" ++ [c] ++ "'")
hLexInStr s (c : rest) = hLexInStr (s ++ [c]) rest
hLexInStr _ [] = Left "String with no closing quote"

alphaChars :: String
alphaChars = ['a' .. 'z'] ++ ['A' .. 'Z']

digitChars :: String
digitChars = ['0' .. '9']

symChars :: String
symChars = alphaChars ++ digitChars ++ ['_', '-', '+']

hLexInSym :: String -> String -> Either String [Token]
hLexInSym s [] = Right $ [Sym s]
hLexInSym s (c : rest)
    | c `elem` symChars = hLexInSym (s ++ [c]) rest
    | otherwise = insertToken (Sym s) rest

hLexNum :: Double -> String -> Either String [Token]
hLexNum n [] = Right $ [Nmb n]
hLexNum n s@(c : rest)
    | c `elem` digitChars = hLexNum ((n * 10) + (fromIntegral $ digitToInt c)) rest
    | otherwise = insertToken (Nmb n) s

hLex :: String -> Either String [Token]
-- hLex s | trace (show s) False = undefined
hLex [] = Right []
hLex s@(c : rest)
    | c == '(' = insertToken Open rest
    | c == ')' = insertToken Close rest
    | c == ' ' = hLex rest
    | c == '"' = hLexInStr "" rest
    | c `elem` digitChars = hLexNum 0 s
    | otherwise = hLexInSym "" s
