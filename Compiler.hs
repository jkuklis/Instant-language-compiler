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


chC e1 e2 m = (checkCorrect e1 m) && (checkCorrect e2 m)


checkCorrect :: Exp -> M.Map String Integer -> Bool
checkCorrect (ExpVar (Ident id)) m = case (M.lookup id m) of
    Just _ -> True
    Nothing -> False
checkCorrect (ExpLit _) m = True
checkCorrect (ExpAdd e1 e2) m = chC e1 e2 m
checkCorrect (ExpSub e1 e2) m = chC e1 e2 m
checkCorrect (ExpMul e1 e2) m = chC e1 e2 m
checkCorrect (ExpDiv e1 e2) m = chC e1 e2 m


calcLoc :: Program -> M.Map String Integer -> Integer -> (M.Map String Integer, Integer)
calcLoc (Prog []) m count = (m, count)

calcLoc (Prog (st:sts)) m count = case st of
    SAss (Ident id) e ->
        let 
            pos = M.lookup id m
            count' = case pos of
                Just _ -> count
                Nothing -> count + 1
            m' = case pos of
                Just _ -> m
                Nothing -> M.insert id count m
            check = checkCorrect e m
        in calcLoc (Prog sts) m' count'
    SExp e ->
        let
            check = checkCorrect e m
        in calcLoc (Prog sts) m count


cD e1 e2 =
    let 
        d1 = calcDepth e1 
        d2 = calcDepth e2
        inc = if d1 == d2
            then 1
            else 0
    in (max d1 d2) + inc

calcDepth :: Exp -> Integer
calcDepth (ExpLit _) = 1
calcDepth (ExpVar _) = 1
calcDepth (ExpAdd e1 e2) = cD e1 e2
calcDepth (ExpSub e1 e2) = cD e1 e2
calcDepth (ExpMul e1 e2) = cD e1 e2
calcDepth (ExpDiv e1 e2) = cD e1 e2

calcDepth_h (SAss _ e) = calcDepth e
calcDepth_h (SExp e) = calcDepth e

calcMaxDepth :: Program -> Integer
calcMaxDepth (Prog sts) = foldl max 0 $ map calcDepth_h sts


compileExps e1 e2 m op =
    let
        (gC1, c1) = compileExp2 e1 m
        (gC2, c2) = compileExp2 e2 m
        c = if c1 == c2
            then c1 + 1
            else max c1 c2
        gC = if c2 > c1
            then case op of
                Add -> gC1 ++ gC2
                Mul -> gC1 ++ gC2
                _ -> gC2 ++ gC1
            else gC2 ++ gC1
        oper = case op of
            Add -> "\tiadd"
            Sub -> "\tisub"
            Mul -> "\timul"
            Div -> "\tidiv"
    in (oper : gC, c)


compileExp2 :: Exp -> M.Map String Integer -> ([String], Integer)
compileExp2 (ExpLit (-1)) m = (["\ticonst_m1"], 1)
compileExp2 (ExpLit int) m = if 0 <= int && int <= 5
    then (["\ticonst_" ++ (show int)], 1)
    else if (-128) <= int && int <= 127
        then (["\tbipush " ++ (show int)], 1)
        else if (-32768 <= int && int <= 32767)
            then (["\tsipush " ++ (show int)], 1)
            else (["\tldc " ++ (show int)], 1)

compileExp2 (ExpVar (Ident id)) m =
    let
        Just pos = M.lookup id m
    in if pos <= 3
        then (["\tiload_" ++ (show pos)], 1)
        else (["\tiload " ++ (show pos)], 1)

compileExp2 (ExpAdd e1 e2) m = compileExps e1 e2 m Add
compileExp2 (ExpSub e1 e2) m = compileExps e1 e2 m Sub
compileExp2 (ExpMul e1 e2) m = compileExps e1 e2 m Mul
compileExp2 (ExpDiv e1 e2) m = compileExps e1 e2 m Div

compile2 :: Program -> M.Map String Integer -> [String] -> [String]
compile2 (Prog []) m textR = textR

compile2 (Prog (st:sts)) m textR = case st of
    SAss (Ident id) e ->
        let
            (gc,_) = compileExp2 e m
            Just pos = M.lookup id m
            storeLine = "\tistore_" ++ (show pos)
            textR' = storeLine : (gc ++ textR)
        in compile2 (Prog sts) m textR'
    SExp e ->
        let
            (gc,_) = compileExp2 e m
            streamLine = "\tgetstatic java/lang/System/out Ljava/io/PrintStream;"
            -- long?
            printLine = "\tinvokevirtual java/io/PrintStream/println(I)V"
            textR' = printLine : (gc ++ [streamLine] ++ textR)
        in compile2 (Prog sts) m textR'


main3 input = do
    let lexed = myLexer input in case pProgram lexed of
        Bad s -> putStrLn "Bad program"
        Ok p -> do
            let 
                initState = M.insert "_counter" 0 M.empty
                (varMap, count) = calcLoc p M.empty 0
                limitLocals = ".limit locals " ++ (show (count + 1))
                maxDepth = (calcMaxDepth p) + 1
                limitStack = ".limit stack " ++ (show maxDepth)
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
                res = unlines $ reverse $ endCode : (compile2 p varMap [limitStack, limitLocals, initCode])
            writeFile "out.j" res
            putStr res


main2 input = do
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

main = do
    input <- getContents
    main2 input
    main3 input
