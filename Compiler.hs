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
    genCode :: [String],
    retValue :: String,
    newCounter :: Integer
}

compileExpr :: Exp -> Integer -> CompExpr
compileExpr e counter = case e of
    ExpVar (Ident id) -> 
        CompExpr{
            genCode = [],
            retValue = "%" ++ id,
            newCounter = counter
        }
    ExpLit int ->
        CompExpr{
            genCode = [],
            retValue = show int,
            newCounter = counter
        }
    ExpAdd e1 e2 ->
        let 
            cE1 = compileExpr e1 counter
            cE2 = compileExpr e2 $ newCounter cE1
            nC2 = newCounter cE2
            newAssign = "\t%" ++ (show nC2) ++ " = add i64 " ++ (retValue cE1) ++ ", " ++ (retValue cE2) 
        in CompExpr{
            genCode = newAssign : ((genCode cE1) ++ (genCode cE2)),
            newCounter = nC2 + 1,
            retValue = "%" ++ (show nC2)
        }


callPrint arg = "\tcall void @printInt(i64 " ++ arg ++ ")"

compile :: Program -> Integer -> [String] -> [String]
compile (Prog []) counter textR = textR

compile (Prog (st:sts)) counter textR = do
    case st of
        SAss (Ident id) e -> []
        SExp e ->  
            let 
                cE = compileExpr e counter
                gC = genCode cE                
                nC = newCounter cE
                rV = retValue cE
                textR' = (callPrint rV) : (gC ++ textR)
            in compile (Prog sts) nC textR'


main = do
    input <- getContents
    let lexed = myLexer input in case pProgram lexed of
        Bad s -> putStrLn "Bad program"
        Ok p -> do
            let 
                initState = M.insert "_counter" 0 M.empty
                initCode = 
                    "declare void @printInt(i64)\n\
                    \define i32 @main() {\n\
                    \entry:"
                endCode = 
                    "\tret i32 0\n\
                    \}"
                res = unlines $ reverse $ endCode : (compile p 0 [initCode])
            writeFile "out.ll" res
            putStr res
