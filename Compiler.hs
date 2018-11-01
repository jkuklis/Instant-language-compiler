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


data CompExpr = CompExpr {
    genCode :: String,
    retValue :: String,
    newCounter :: Integer
}

compileExpr :: Exp -> Integer -> CompExpr
compileExpr e counter = case e of
    ExpVar (Ident id) -> 
        CompExpr{
            genCode = "",
            retValue = "%" ++ id,
            newCounter = counter
        }
    ExpLit int ->
        CompExpr{
            genCode = "",
            retValue = show int,
            newCounter = counter
        }
    ExpAdd e1 e2 ->
        let 
            cE1 = compileExpr e1 counter
            cE2 = compileExpr e2 $ newCounter cE1
        in cE1        


callPrint arg = "call void @printInt(i32 %" ++ arg ++ ")\n"

compile :: Program -> Integer -> String -> String
compile (Prog []) counter textR = textR

compile (Prog (st:sts)) counter textR = do
    case st of
        SAss (Ident id) e -> "End"
        SExp e -> do    
            let textR' = textR ++ (callPrint $ show counter)
            compile (Prog sts) counter textR'


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
            putStr initCode
            putStr $ compile p 0 ""
            putStr endCode
