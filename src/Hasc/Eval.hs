module Hasc.Eval where

import qualified Data.Map as Map

import Data.Either.Utils

-- import Debug.Trace
import Hasc.Lex

data Primitive = Plus | Subtract | Sin | Cos deriving (Show)

data Special = SIf | SDo | SLambda | SLet deriving (Show)

{-
    So SLambda is the special form. Evaluating this gives you an ELambda, which
    is the expression.
    This has exactly the same semantics as Primitive (evaluate all args, then
    apply the Callable to the resulting expression list), but I'm struggling
    to share code.
-}
data Lambda = Lambda {lEnv :: Env, lArgs :: [String], lBody :: Expr} deriving (Show)

data Expr
    = Atom Val
    | HList [Expr]
    | Primitive Primitive
    | Special Special
    | ELambda Lambda

--    deriving (Show)

class Callable c where
    invoke :: c -> Env -> [Expr] -> Either String Expr

instance Show Expr where
    show (Atom v) = show v
    show (HList es) = "(" ++ (unwords $ map show es) ++ ")"
    show (Primitive p) = "<primitive: " ++ (show p) ++ ">"
    show (Special s) = "<special: " ++ (show s) ++ ">"
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
    invoke Subtract _ es = do
        nums <- maybeToEither "Non-numeric args" $ sequence $ map asNum es
        return $ case nums of
            (n : rest) -> Atom $ Nbr $ (n - sum rest)
            [] -> Atom $ Nbr $ 0
    invoke Sin _ [(Atom (Nbr theta))] = Right $ Atom $ Nbr $ sin theta
    invoke Sin _ _ = Left "sin takes one numeric argument"
    invoke Cos _ [(Atom (Nbr theta))] = Right $ Atom $ Nbr $ cos theta
    invoke Cos _ _ = Left "cos takes one numeric argument"

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
    -- Rewrite to lambda form
    -- (let ((k1 v2) (k2 v2)) body)
    -- => ((lambda (k1 k2) body) v1 v2)
    invoke SLet env [(HList bindings), body] = do
        pairs <- sequence $ map (letParseOne env) bindings
        let ids = map (Atom . Sym . fst) pairs
        let args = map snd pairs
        eval env $ HList $ [HList [Atom $ Sym "lambda", HList ids, body]] ++ args
    invoke SLet _ _ = Left "let must have list-of-bindings and body"

letParseOne :: Env -> Expr -> Either String (String, Expr)
letParseOne env (HList [(Atom (Sym s)), v]) = do
    v' <- eval env v
    return (s, v')
letParseOne _ _ = Left "let-binding must be a (k v) list"

mkDefaultEnv :: Env
mkDefaultEnv =
    [ Map.fromList
        [ ("pi", Atom $ Nbr pi)
        , -- Callable primitives
          ("+", Primitive Plus)
        , ("-", Primitive Subtract)
        , ("sin", Primitive Sin)
        , ("cos", Primitive Cos)
        , -- Special forms, get unevaluated args
          ("if", Special SIf)
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
-- eval _ e | (trace $ "EVAL: " ++ (show e)) False = undefined
eval env (Atom (Sym s)) = envLookup env s
eval _ (Atom a) = Right $ Atom a
eval env (HList (ef : eargs)) = do
    -- traceM ("HList:" ++ show e)
    f <- eval env ef
    res <- case f of
        Primitive p -> do
            -- traceM "Primitive:"
            args <- sequence $ map (eval env) eargs
            return $ invoke p env args
        ELambda l -> do
            -- traceM "ELambda:"
            args <- sequence $ map (eval env) eargs
            return $ invoke l env args
        -- Same as Primitive, but we pass env and don't pre-evaluate args
        Special s -> Right $ invoke s env eargs
        _ -> Left $ "Non-callable in callable position: " ++ show f
    res
eval _ (HList []) = Left "Eval empty list"
eval _ (Primitive _) = Left "Can't evaluate primitive directly"
eval _ (Special _) = Left "Can't evaluate callable directly"
eval _ (ELambda _) = Left "Can't evaluate lambda obj"
