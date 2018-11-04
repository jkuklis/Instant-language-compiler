{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Exit ( exitFailure, exitSuccess )

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import AbsInstant
import ParInstant
import ErrM


type Exception = String

data Operation = Add | Sub | Mul | Div

data ExpLLVM = ExpLLVM {
    genInstr :: [String],
    retVal :: String,
    newCounter :: Integer
}

invalidVars :: Exp -> M.Map String Integer -> S.Set String

invalidVars e m = let 
    invVars_h e1 e2 m = S.union (invalidVars e1 m) $ invalidVars e2 m 
    in case e of
    (ExpLit _) -> S.empty
    (ExpVar (Ident id)) -> case M.lookup id m of
        Nothing -> S.singleton id
        _ -> S.empty
    (ExpAdd e1 e2) -> invVars_h e1 e2 m
    (ExpSub e1 e2) -> invVars_h e1 e2 m
    (ExpMul e1 e2) -> invVars_h e1 e2 m
    (ExpDiv e1 e2) -> invVars_h e1 e2 m


getVariables :: Program -> Integer -> [String] -> State (M.Map String Integer) [String]

getVariables (Prog []) _ invalidSt = return invalidSt
getVariables (Prog (st:stmts)) line invalidSt = 
    let result invV = getVariables (Prog stmts) (line + 1) (if S.null invV
        then invalidSt
        else ((unwords (map (\x -> x ++ ",") (S.toList invV))) ++ " in line " ++ (show line)) : invalidSt)
    in case st of
    SAss (Ident id) e -> do
        invalidV <- gets $ invalidVars e
        pos <- gets $ M.lookup id
        count <- gets $ M.size
        modify (case pos of
            Nothing -> M.insert id (toInteger count)
            _ -> \x -> x)
        result invalidV
    SExp e -> do
        invalidV <- gets $ invalidVars e
        result invalidV


depth :: Exp -> Integer

depth e = let 
    depth_h e1 e2 = let
        d1 = depth e1
        d2 = depth e2
        inc = if d1 == d2
            then 1
            else 0
        in (max d1 d2) + inc
    in case e of   
    (ExpLit _) -> 1
    (ExpVar _) -> 1
    (ExpAdd e1 e2) -> depth_h e1 e2
    (ExpSub e1 e2) -> depth_h e1 e2
    (ExpMul e1 e2) -> depth_h e1 e2
    (ExpDiv e1 e2) -> depth_h e1 e2


calcMaxDepth :: Program -> Integer

calcMaxDepth (Prog stmts) = foldl max 0 $ map (\st -> case st of
    SAss _ e -> depth e
    SExp e -> depth e) stmts


compileExpJVM :: Exp -> Reader (M.Map String Integer) ([String], Integer)

compileExpJVM e = let
    cExpJVM_h e1 e2 op = do
        (genInstr1, count1) <- compileExpJVM e1
        (genInstr2, count2) <- compileExpJVM e2
        let
            operLine = case op of
                Add -> "\tiadd"
                Sub -> "\tisub"
                Mul -> "\timul"
                Div -> "\tidiv"
            addAndMulCase = if count2 > count1
                then (genInstr1 ++ genInstr2, count2)
                else (genInstr2 ++ genInstr1, count2 + (if count1 == count2 then 1 else 0))
            (instr, count) = case op of
                Add -> addAndMulCase
                Mul -> addAndMulCase
                _ -> (genInstr2 ++ genInstr1, count2 + (if count1 <= count2 then 1 else 0)) 
        return (operLine : instr, count)
    in case e of
    ExpLit int
        | int == -1 -> return (["\ticonst_m1"], 1)
        | 0 <= int && int <= 5 -> return (["\ticonst_" ++ (show int)], 1)
        | -128 <= int && int <= 127 -> return (["\tbipush " ++ (show int)], 1)
        | -32768 <= int && int <= 32767 -> return (["\tsipush " ++ (show int)], 1)
        | otherwise -> return (["\tldc " ++ (show int)], 1)
    ExpVar (Ident id) -> do
        Just pos <- asks $ M.lookup id
        if pos <= 3
            then return (["\tiload_" ++ (show pos)], 1)
            else return (["\tiload " ++ (show pos)], 1)
    ExpAdd e1 e2 -> cExpJVM_h e1 e2 Add
    ExpSub e1 e2 -> cExpJVM_h e1 e2 Sub
    ExpMul e1 e2 -> cExpJVM_h e1 e2 Mul
    ExpDiv e1 e2 -> cExpJVM_h e1 e2 Div


compileJVM :: Program -> [String] -> Reader (M.Map String Integer) [String]

compileJVM (Prog []) instr = return instr
compileJVM (Prog (st:stmts)) instr = case st of
    SAss (Ident id) e -> do
        Just pos <- asks $ M.lookup id
        let storeLine = "\tistore_" ++ (show pos)
        (genInstr, _) <- asks $ runReader $ compileExpJVM e 
        compileJVM (Prog stmts) ([storeLine] ++ genInstr ++ instr) 
    SExp e -> do
        let streamLine = "\tgetstatic java/lang/System/out Ljava/io/PrintStream;"
        let printLine = "\tinvokevirtual java/io/PrintStream/println(I)V" -- long?
        (genInstr, _) <- asks $ runReader $ compileExpJVM e
        compileJVM (Prog stmts) ([printLine] ++ genInstr ++ [streamLine] ++ instr) 


createFileJVM :: Program -> M.Map String Integer -> IO ()

createFileJVM p locals =
    let
        initCode =
            [".class  public Main",
            ".super java/lang/Object\n",
            "; standard initializer",
            ".method public <init>()V",
            "\taload_0\n",
            "\tinvokenonvirtual java/lang/Object/<init>()V",
            "\treturn",
            ".end method\n",
            ".method public static main([Ljava/lang/String;)V"]
        endCode =
            ["\treturn",
            ".end method"]
        localsLimit = (M.size locals) + 1
        localsLine = ".limit locals " ++ (show localsLimit)
        stackLimit = (calcMaxDepth p) + 1
        stackLine = ".limit stack " ++ (show stackLimit)
        instrCode = runReader (compileJVM p []) locals
        jasminOut = unlines $ initCode ++ [localsLine, stackLine] ++ (reverse instrCode) ++ endCode
    in do
        writeFile "out.j" jasminOut
        putStr jasminOut


compileExpLLVM :: Exp -> Integer -> Reader (M.Map String Integer) ExpLLVM

compileExpLLVM e counter = let
    cExpLLVM_h e1 e2 counter op = do
        exp1 <- compileExpLLVM e1 counter
        exp2 <- compileExpLLVM e2 $ newCounter exp1
        let counter2 = newCounter exp2
            retV1 = retVal exp1
            retV2 = retVal exp2
            oper = case op of
                Add -> "add"
                Sub -> "sub"
                Mul -> "mul"
                Div -> "sdiv"
        let assignLine = "\t%" ++ (show counter2) ++ " = " ++ oper ++ " i64 " ++ retV1 ++ ", " ++ retV2 
        return ExpLLVM{
            genInstr = [assignLine] ++ (genInstr exp2) ++ (genInstr exp1),
            newCounter = counter2 + 1,
            retVal = "%" ++ (show counter2)
        }
    in case e of
    ExpLit int -> return
        ExpLLVM{
            genInstr = [],
            retVal = show int,
            newCounter = counter
        }
    ExpVar (Ident id) -> do
        Just pos <- asks $ M.lookup id
        return ExpLLVM{
            genInstr = [],
            retVal = "%" ++ id ++ "_" ++ (show pos),
            newCounter = counter
        }
    ExpAdd e1 e2 -> cExpLLVM_h e1 e2 counter Add
    ExpSub e1 e2 -> cExpLLVM_h e1 e2 counter Sub
    ExpMul e1 e2 -> cExpLLVM_h e1 e2 counter Mul
    ExpDiv e1 e2 -> cExpLLVM_h e1 e2 counter Div


compileLLVM :: Program -> Integer -> [String] -> State (M.Map String Integer) [String]

compileLLVM (Prog []) _ instr = return instr
compileLLVM (Prog (st:stmts)) counter instr = case st of
    SAss (Ident id) e -> do
        expLLVM <- gets $ runReader $ compileExpLLVM e counter
        let nCounter = newCounter expLLVM
        let retV = retVal expLLVM
        Just pos <- gets $ M.lookup id
        let assignLine = "\t%" ++ id ++ "_" ++ (show (pos + 1)) ++ " = add i64 0, " ++ retV
        modify $ M.insert id (pos + 1)        
        compileLLVM (Prog stmts) nCounter ([assignLine] ++ (genInstr expLLVM) ++ instr)
    SExp e -> do
        expLLVM <- gets $ runReader $ compileExpLLVM e counter
        let nCounter = newCounter expLLVM
        let retV = retVal expLLVM
        let printLine = "\tcall void @printInt(i64 " ++ retV ++ ")"
        compileLLVM (Prog stmts) nCounter ([printLine] ++ (genInstr expLLVM) ++ instr)


createFileLLVM :: Program -> M.Map String Integer -> IO ()

createFileLLVM p locals =
    let 
        initCode =
            ["declare void @printInt(i64)",
            "define i32 @main() {",
            "entry:"]
        endCode = 
            ["\tret i32 0",
            "}"]
        instrCode = evalState (compileLLVM p 0 []) locals
        llOut = unlines $ initCode ++ (reverse instrCode) ++ endCode
    in do
        writeFile "out.ll" llOut
        putStr llOut


processAndCompile :: IO ()

processAndCompile = do
    input <- getContents
    let tokens = myLexer input in case pProgram tokens of
        Bad error -> do 
            putStrLn "Failure to parse program!"
            putStrLn error
            putStrLn "Tokens:"
            putStrLn $ show tokens
            exitFailure
        Ok prog -> let 
            (invalidSt, locals) = runState (getVariables prog 0 []) M.empty
            in if null invalidSt 
            then do
                createFileJVM prog locals
                putStrLn ""
                createFileLLVM prog $ M.map (\x -> 0) locals 
                exitSuccess
            else do
                putStrLn "Failure, using undeclared variables:"
                putStr $ unlines $ reverse invalidSt
                exitFailure

main = processAndCompile
