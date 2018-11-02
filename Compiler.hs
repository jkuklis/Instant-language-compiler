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


data Operation = Add | Sub | Mul | Div


compileOper :: (M.Map String Integer) -> Exp -> Exp -> Integer -> Operation -> CompExpr
compileOper m e1 e2 counter op = 
    let 
        cE1 = compileExpr e1 m counter
        cE2 = compileExpr e2 m $ newCounter cE1
        nC2 = newCounter cE2
        oper = case op of
            Add -> "add"
            Sub -> "sub"
            Mul -> "mul"
            Div -> "sdiv"
        newAssign = "\t%" ++ (show nC2) ++ " = " ++ oper ++ " i64 " ++ (retValue cE1) ++ ", " ++ (retValue cE2) 
    in CompExpr{
        genCode = newAssign : ((genCode cE2) ++ (genCode cE1)),
        newCounter = nC2 + 1,
        retValue = "%" ++ (show nC2)
    }

compileExpr :: Exp -> (M.Map String Integer) -> Integer -> CompExpr
compileExpr e m counter = case e of
    ExpVar (Ident id) -> 
        let Just v = M.lookup id m
        in CompExpr{
            genCode = [],
            retValue = "%" ++ id ++ "_" ++ (show v),
            newCounter = counter
        }
    ExpLit int ->
        CompExpr{
            genCode = [],
            retValue = show int,
            newCounter = counter
        }
    ExpAdd e1 e2 -> compileOper m e1 e2 counter Add
    ExpSub e1 e2 -> compileOper m e1 e2 counter Sub
    ExpMul e1 e2 -> compileOper m e1 e2 counter Mul
    ExpDiv e1 e2 -> compileOper m e1 e2 counter Div


callPrint arg = "\tcall void @printInt(i64 " ++ arg ++ ")"

compile :: Program -> Integer -> (M.Map String Integer) -> [String] -> [String]
compile (Prog []) counter m textR = textR

compile (Prog (st:sts)) counter m textR = do
    case st of
        SAss (Ident id) e ->
            let 
                cE = compileExpr e m counter
                gC = genCode cE
                nC = newCounter cE
                rV = retValue cE
                which = M.lookup id m
                nWhich = case which of
                    Just v -> v + 1
                    Nothing -> 0
                m' = M.insert id nWhich m
                textR' = ("\t%" ++ id ++ "_" ++ (show nWhich) ++ " = add i64 0, " ++ rV) : (gC ++ textR)
            in compile (Prog sts) nC m' textR'
        SExp e ->
            let 
                cE = compileExpr e m counter
                gC = genCode cE                
                nC = newCounter cE
                rV = retValue cE
                textR' = (callPrint rV) : (gC ++ textR)
            in compile (Prog sts) nC m textR' 



compile2 :: Program -> (M.Map String Integer) -> [String] -> [String]


exprSize :: Exp -> Integer



stackSize :: Program -> Integer -> Integer
stackSize (Prog []) acc -> acc

stackSize (Prog (st:sts)) acc ->


main = do
    input <- getContents
    let lexed = myLexer input in case pProgram lexed of
        Bad s -> putStrLn "Bad program"
        Ok p -> do
            let 
                initState = M.insert "_counter" 0 M.empty
                initCode = 
                    ".class  public Hello\n\
                    \.super  java/lang/Object\n\n\
                    \; standard initializer\n\
                    \.method public <init>()V\n\
                    \\taload_0\n\n\
                    \\tinvokenonvirtual java/lang/Object/<init>()V\n\
                    \\treturn\n\
                    \.end method\n\n\
                    \.method public static main([Ljava/lang/String;)V"
                endCode = 
                    "\treturn\n\
                    \.end method"
                res = unlines $ reverse $ endCode : (compile p 0 M.empty [initCode])
            writeFile "out.j" res
            putStr res


main2 = do
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
                res = unlines $ reverse $ endCode : (compile p 0 M.empty [initCode])
            writeFile "out.ll" res
            putStr res
