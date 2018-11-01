{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import AbsInstant
import LexInstant
import SkelInstant
import ParInstant
import ErrM

getCounter :: State (M.Map String Integer) (Maybe Integer)
getCounter = gets $ M.lookup "_counter"

-- compileExpr :: Exp -> State (M.Map String Integer) (Integer, String)
-- compileExpr 


callPrint arg = "call void @printInt(i32 %" ++ arg ++ ")\n"

compile :: Program -> String -> State (M.Map String Integer) String
compile (Prog []) textR = return textR

compile (Prog (st:sts)) textR = do
    case st of
        SAss (Ident id) e -> return "End"
        SExp e -> do 
            Just counter <- getCounter     
            let textR' = textR ++ (callPrint $ show counter)
            compile (Prog sts) textR'


main = do
    input <- getContents
    let lexed = myLexer input in case pProgram lexed of
        Bad s -> putStrLn "Bad program"
        Ok p -> do
            let 
                initState = M.insert "_counter" 0 M.empty
                initCode = 
                    "declare void @printInt(i32)\n\
                    \define i32 @main() {\n\
                    \entry:\n"
                endCode = 
                    "\tret i32 0\n\
                    \}\n"
            putStr $ evalState (compile p initCode) initState
            putStr endCode
