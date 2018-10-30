module Main where

import Control.Monad.State
import qualified Data.Map as M

import AbsInstant
import LexInstant
import SkelInstant
import ParInstant
import ErrM


eval (ExpLit i) m = i
eval (ExpVar (Ident id)) m = let Just v = M.lookup id m in v
eval (ExpDiv e1 e2) m =
    let
        v1 = eval e1 m
        v2 = eval e2 m
    in v1 `div` v2
eval (ExpMul e1 e2) m =
    let
        v1 = eval e1 m
        v2 = eval e2 m
    in v1 * v2
eval (ExpSub e1 e2) m =
    let
        v1 = eval e1 m
        v2 = eval e2 m
    in v1 - v2
eval (ExpAdd e1 e2) m =
    let
        v1 = eval e1 m
        v2 = eval e2 m
    in v1 + v2
    

interpret (Prog []) m = putStrLn "End"
interpret (Prog (stat:sts)) m = do 
    case stat of
        SAss (Ident id) e ->
            let v = eval e m in do
            putStr id
            putStr " = "
            putStrLn (show v)
            interpret (Prog sts) (M.insert id v m)
        SExp e -> let v = eval e m in do 
            putStrLn (show v)
            interpret (Prog sts) m

main = do
    input <- getContents
    let lexed = myLexer input in case pProgram lexed of
        Bad s -> putStrLn "Bad program"
        Ok p -> interpret p M.empty
