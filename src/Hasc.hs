module Hasc
where

-- data Token = Open | Close | Sym {symVal :: String} | Nmb {nmbVal :: String} | Str {strVal :: String} deriving (Show, Eq)
data Token = Open | Close | Sym {symVal :: String} | Str {strVal :: String} deriving (Show, Eq)

hLexInStr :: String -> String -> Either String [Token]
hLexInStr s ('"' : rest) = fmap ((Str s) :) (hLex rest)
hLexInStr _ ('\\' : "") = Left "Backslash in string with no closing quote"
hLexInStr s ('\\' : '\\' : rest) = hLexInStr (s ++ "\\") rest
hLexInStr s ('\\' : '"' : rest) = hLexInStr (s ++ "\"") rest
hLexInStr _ ('\\' : c : _) = Left ("Unknown backslash escape" ++ [c])
hLexInStr s (c : rest) = hLexInStr (s ++ [c]) rest
hLexInStr _ [] = Left "String with no closing quote"

hLexInSym :: String -> String -> Either String [Token]
hLexInSym s (' ' : rest) = fmap ((Sym s) :) (hLex rest)
hLexInSym s (c : rest) = hLexInSym (s ++ [c]) rest
hLexInSym s [] = Right $ [Sym s]

hLex :: String -> Either String [Token]
hLex [] = Right []
hLex ('(' : rest) = fmap (Open :) (hLex rest)
hLex (')' : rest) = fmap (Close :) (hLex rest)
hLex (' ' : rest) = hLex rest
hLex ('"' : rest) = hLexInStr "" rest
hLex (c : rest) = fmap ((Sym (c : "")) :) (hLex rest)
