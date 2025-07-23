module Hasc
where

import Data.Char

-- data Token = Open | Close | Sym {symVal :: String} | Nmb {nmbVal :: String} | Str {strVal :: String} deriving (Show, Eq)
data Token = Open | Close | Sym {symVal :: String} | Nmb {nmbVal :: Double} | Str {strVal :: String} deriving (Show, Eq)

hLexInStr :: String -> String -> Either String [Token]
hLexInStr s ('"' : rest) = fmap ((Str s) :) (hLex rest)
hLexInStr _ ('\\' : "") = Left "Backslash in string with no closing quote"
hLexInStr s ('\\' : '\\' : rest) = hLexInStr (s ++ "\\") rest
hLexInStr s ('\\' : '"' : rest) = hLexInStr (s ++ "\"") rest
hLexInStr _ ('\\' : c : _) = Left ("Unknown backslash escape" ++ [c])
hLexInStr s (c : rest) = hLexInStr (s ++ [c]) rest
hLexInStr _ [] = Left "String with no closing quote"

alphaChars :: String
alphaChars = ['a' .. 'z'] ++ ['A' .. 'Z']

digitChars :: String
digitChars = ['0' .. '9']

symChars :: String
symChars = alphaChars ++ digitChars ++ ['_', '-']

hLexInSym :: String -> String -> Either String [Token]
hLexInSym s [] = Right $ [Sym s]
hLexInSym s (c : rest)
    | c `elem` symChars = hLexInSym (s ++ [c]) rest
    | otherwise = fmap ((Sym s) :) (hLex rest)

hLexNum :: Double -> String -> Either String [Token]
hLexNum n [] = Right $ [Nmb n]
hLexNum n (c : rest)
    | c `elem` digitChars = hLexNum ((n * 10) + (fromIntegral $ digitToInt c)) rest
    | otherwise = fmap (Nmb n :) (hLex rest)

hLex :: String -> Either String [Token]
hLex [] = Right []
hLex s@(c : rest)
    | c == '(' = fmap (Open :) (hLex rest)
    | c == ')' = fmap (Close :) (hLex rest)
    | c == ' ' = hLex rest
    | c == '"' = hLexInStr "" rest
    | c `elem` digitChars = hLexNum 0 s
    | otherwise = hLexInSym "" s
