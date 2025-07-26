module Hasc.Env where

import qualified Data.Map as Map

import Data.Either.Utils
import Debug.Trace
import Hasc.Lex
import Hasc.Parse

type Frame = Map.Map String Expr
type Env = [Frame]

plus :: [Expr] -> Either String Expr
plus es =
    let mvs = sequence $ map asNum es
    in (trace $ show es) $ fmap (Atom . Val . Nbr . sum) $ maybeToEither "Non-numeric args" mvs

mkDefaultEnv :: Env
mkDefaultEnv = [Map.fromList [("+", Callable plus)]]

envLookup :: Env -> String -> Either String Expr
envLookup [] s = Left $ "Unknown symbol: " ++ s
envLookup (f : rest) s =
    let e = Map.lookup s f
    in case e of
        Nothing -> envLookup rest s
        Just v -> Right v
