module Hasc.Env where

import qualified Data.Map as Map

import Hasc.Parse

type Frame = Map.Map String Expr
type Env = [Frame]

mkDefaultEnv :: Env
-- mkDefaultEnv = [Map.fromList ["+" : Callable plus]]
mkDefaultEnv = []

envLookup :: Env -> String -> Either String Expr
envLookup [] s = Left $ "Unknown symbol: " ++ s
envLookup (f : rest) s =
    let e = Map.lookup s f
    in case e of
        Nothing -> envLookup rest s
        Just v -> Right v
