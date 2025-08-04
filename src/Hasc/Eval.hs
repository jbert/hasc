module Hasc.Eval where

import qualified Data.Map as Map

import Data.Either.Utils

-- import Debug.Trace
import Hasc.Lex

data Primitive = Plus

data Special = SIf | SDo | SLambda | SLet

{-
    So SLambda is the special form. Evaluating this gives you an ELambda, which
    is the expression.
    This has exactly the same semantics as Primitive (evaluate all args, then
    apply the Callable to the resulting expression list), but I'm struggling
    to share code.
-}
data Lambda = Lambda {lEnv :: Env, lArgs :: [String], lBody :: Expr}

data Expr
    = Atom Val
    | HList [Expr]
    | Primitive Primitive
    | Special Special
    | ELambda Lambda

class Callable c where
    invoke :: c -> Env -> [Expr] -> Either String Expr

instance Show Expr where
    show (Atom v) = show v
    show (HList es) = "(" ++ (unwords $ map show es) ++ ")"
    show (Primitive _) = "<primitive>"
    show (Special _) = "<special>"
    show (ELambda (Lambda _ args _)) = "<lambda: (" ++ (unwords args) ++ ")>"

instance Eq Expr where
    (==) (Atom a) (Atom b) = a == b
    (==) (HList a) (HList b) = and $ [(length a == length b)] ++ zipWith (==) a b
    (==) _ _ = False

asNum :: Expr -> Maybe Double
asNum (Atom (Nbr x)) = Just x
asNum _ = Nothing

asString :: Expr -> Maybe String
asString (Atom (Str x)) = Just x
asString (Atom (Sym x)) = Just x
asString _ = Nothing

asBool :: Expr -> Maybe Bool
asBool (Atom (Bol v)) = Just v
-- Although this is an opportunity for 'truthy' values
asBool _ = Nothing

type Frame = Map.Map String Expr
type Env = [Frame]

instance Callable Primitive where
    invoke Plus _ es =
        let mvs = sequence $ map asNum es
        in fmap (Atom . Nbr . sum) $ maybeToEither "Non-numeric args" mvs

instance Callable Lambda where
    invoke (Lambda cenv argStrs body) _ args =
        let
            frame = Map.fromList $ zip argStrs args
            env = frame : cenv
        in
            eval env body

instance Callable Special where
    invoke SIf env [epred, eth, eel] = do
        let mp = asBool epred
        case mp of
            Just p ->
                let v = if p then eth else eel
                in eval env v
            Nothing -> Left "if predicate must be boolean"
    invoke SIf _ _ = Left "if must have 3-arity"
    invoke SDo env (arg : rest) = do
        es <- sequence $ map (eval env) (arg : rest)
        return $ last es
    invoke SDo _ [] = Left "Empty 'do' form"
    invoke SLambda env [(HList args), body] =
        let syms = [s | Atom (Sym s) <- args]
        in Right $ ELambda (Lambda env syms body)
    invoke SLambda _ _ = Left "lambda must have list-of-args and body"
    -- Could do this as a lambda?
    invoke SLet env [(HList bindings), body] = do
        frame <- letParseBindings env bindings
        eval (frame : env) body
    invoke SLet _ _ = Left "let must have list-of-bindings and body"

letParseBindings :: Env -> [Expr] -> Either String Frame
letParseBindings env bindings = do
    pairs <- sequence $ map (letParseOne env) bindings
    return $ Map.fromList pairs

letParseOne :: Env -> Expr -> Either String (String, Expr)
letParseOne env (HList [(Atom (Sym s)), v]) = do
    v' <- eval env v
    return (s, v')
letParseOne _ _ = Left "let-binding must be a (k v) list"

mkDefaultEnv :: Env
mkDefaultEnv =
    [ Map.fromList
        [ ("+", Primitive Plus)
        , ("if", Special SIf)
        , ("do", Special SDo)
        , ("lambda", Special SLambda)
        , ("let", Special SLet)
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
eval env (Atom (Sym s)) = envLookup env s
eval _ (Atom a) = Right $ Atom a
eval _ (Primitive _) = Left "Can't evaluate primitive directly"
eval _ (Special _) = Left "Can't evaluate callable directly"
eval _ (ELambda _) = Left "Can't evaluate lambda obj"
eval env (HList (ef : eargs)) = do
    f <- eval env ef
    res <- case f of
        Primitive p -> do
            args <- sequence $ map (eval env) eargs
            return $ invoke p env args
        ELambda l -> do
            args <- sequence $ map (eval env) eargs
            return $ invoke l env args
        -- Same as Primitive, but we pass env and don't pre-evaluate args
        Special s -> Right $ invoke s env eargs
        _ -> Left $ "Non-callable in callable position: " ++ show f
    res
eval _ (HList []) = Left "Eval empty list"
