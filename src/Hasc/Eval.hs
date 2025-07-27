module Hasc.Eval where

import qualified Data.Map as Map

import Data.Either.Utils
import Debug.Trace
import Hasc.Lex

data Expr
    = Atom Token
    | HList [Expr]
    | Callable ([Expr] -> Either String Expr)
    | Special (Env -> [Expr] -> Either String Expr)

instance Show Expr where
    show (Atom t) = show t
    show (HList es) = "(" ++ (unwords $ map show es) ++ ")"
    show (Callable _) = "<callable>"
    show (Special _) = "<special>"

asNum :: Expr -> Maybe Double
asNum (Atom (Val (Nbr x))) = Just x
asNum _ = Nothing

asString :: Expr -> Maybe String
asString (Atom (Val (Str x))) = Just x
asString (Atom (Val (Sym x))) = Just x
asString _ = Nothing

asBool :: Expr -> Maybe Bool
asBool (Atom (Val (Bol v))) = Just v
-- Although this is an opportunity for 'truthy' values
asBool _ = Nothing

type Frame = Map.Map String Expr
type Env = [Frame]

plus :: [Expr] -> Either String Expr
plus es =
    let mvs = sequence $ map asNum es
    in (trace $ show es) $ fmap (Atom . Val . Nbr . sum) $ maybeToEither "Non-numeric args" mvs

hIf :: Env -> [Expr] -> Either String Expr
hIf env [epred, eth, eel] = do
    let mp = asBool epred
    case mp of
        Just p ->
            let v = if p then eth else eel
            in eval env v
        Nothing -> Left "if predicate must be boolean"
hIf _ _ = Left "if must have 3-arity"

mkDefaultEnv :: Env
mkDefaultEnv =
    [ Map.fromList
        [ ("+", Callable plus)
        , ("if", Special hIf)
        ]
    ]

envLookup :: Env -> String -> Either String Expr
envLookup [] s = Left $ "Unknown symbol: " ++ s
envLookup (f : rest) s =
    let e = Map.lookup s f
    in case e of
        Nothing -> envLookup rest s
        Just v -> Right v

eval :: Env -> Expr -> Either String Expr
eval env (Atom (Val (Sym s))) = envLookup env s
eval _ (Atom a) = Right $ Atom a
eval _ (Callable _) = Left "Can't evaluate callable directly"
eval _ (Special _) = Left "Can't evaluate callable directly"
eval env (HList (ef : eargs)) = do
    f <- eval env ef
    res <- case f of
        Callable c -> do
            args <- (sequence $ map (eval env) eargs)
            Right $ c args
        -- Same as callable, but we pass env and don't pre-evaluate args
        Special c -> Right $ c env eargs
        _ -> Left "Non-callable in callable position"
    res
eval _ (HList []) = Left "Eval empty list"
