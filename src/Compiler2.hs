{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment ( getArgs )
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


data Operation = Add | Sub | Mul | Div

data Mode = JVM | LLVM

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
    depth_h e1 e2 swap = let
        d1 = depth e1
        d2 = depth e2
        in (max d1 d2) + (if d1 == d2 then 1 else 0)
    in case e of
    (ExpLit _) -> 1
    (ExpVar _) -> 1
    (ExpAdd e1 e2) -> depth_h e1 e2 True
    (ExpSub e1 e2) -> depth_h e1 e2 False
    (ExpMul e1 e2) -> depth_h e1 e2 True
    (ExpDiv e1 e2) -> depth_h e1 e2 False


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
            (instr, count) = if count2 > count1
                then (genInstr1 ++ genInstr2, count2)
                else (genInstr2 ++ genInstr1, count1 + (if count1 == count2 then 1 else 0))
            swapLine = case op of
                Add -> []
                Mul -> []
                _ -> if count2 > count1
                    then ["\tswap"]
                    else []
        return ([operLine] ++ swapLine ++ instr, count)
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
        let printLine = "\tinvokevirtual java/io/PrintStream/println(I)V"
        (genInstr, _) <- asks $ runReader $ compileExpJVM e
        compileJVM (Prog stmts) ([printLine] ++ genInstr ++ [streamLine] ++ instr) 


createFileJVM :: Program -> M.Map String Integer -> String -> String

createFileJVM p locals fileName =
    let
        initCode =
            [".class public " ++ fileName,
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
    in unlines $ initCode ++ [localsLine, stackLine] ++ (reverse instrCode) ++ endCode


compileExpLLVM :: Exp -> Integer -> Reader (M.Map String String) ExpLLVM

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
        let assignLine = "\t%" ++ (show counter2) ++ " = " ++ oper ++ " i32 " ++ retV1 ++ ", " ++ retV2 
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
            retVal = pos,
            newCounter = counter
        }
    ExpAdd e1 e2 -> cExpLLVM_h e1 e2 counter Add
    ExpSub e1 e2 -> cExpLLVM_h e1 e2 counter Sub
    ExpMul e1 e2 -> cExpLLVM_h e1 e2 counter Mul
    ExpDiv e1 e2 -> cExpLLVM_h e1 e2 counter Div


data CodegenState {
    counter :: Integer,
    genCode :: [String]
    } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


compileExpLLVM :: Program -> Codegen String


appendLine :: String -> CodegenState -> CodegenState

appendLine line state = state {genCode = line : (genCode s)}


emitStore :: String -> String -> Codegen ()

emitStore id val =
    let storeLine = "\tstore i32 " ++ val ++ ", i32* %" ++ id
    in modify $ appendLine storeLine


emitPrint :: String -> Codegen ()

emitPrint val =
    let printLine = "\tcall void @printInt(i32 " ++ val ++ ")"
    in modify $ appendLine

compileLLVM :: Program -> Codegen String

compileLLVM (Prog []) = do
    compiledProg <- gets genCode
    return $ unlines $ reverse compiledProg

compileLLVM (Prog (st:stmts)) = case st of
    SAss (Ident id) e -> do
        retVal <- compileExpLLVM e
        emitStore id retVal
        compileLLVM $ Prog stmts
    SExp e -> do
        retVal <- compileExpLLVM e
        emitPrint retVal
        compileLLVM $ Prog stmts






createFileLLVM :: Program -> M.Map String String -> String

createFileLLVM p locals =
    let 
        initCode =
            ["declare void @printInt(i32)",
            "define i32 @main() {",
            "entry:"]
        endCode = 
            ["\tret i32 0",
            "}"]
        instrCode = evalState (compileLLVM p) 0
    in unlines $ initCode ++ (reverse instrCode) ++ endCode


processAndCompile :: Mode -> String -> Bool -> IO ()
processAndCompile mode fileName debug = do
    input <- getContents
    let tokens = myLexer input in case pProgram tokens of
        Bad error -> do 
            putStrLn "Failure to parse program!"
            putStrLn error
            putStrLn "Tokens:"
            putStrLn $ show tokens
            exitFailure
        Ok prog -> let 
            (invalidSt, locals) = runState (getVariables prog 1 []) M.empty
            in if null invalidSt 
            then do
                let 
                    out = case mode of
                        JVM -> createFileJVM prog locals fileName
                        LLVM -> createFileLLVM prog $ M.map (\x -> "0") locals
                    outFile = case mode of
                        JVM -> "output/out.j"
                        LLVM -> "output/out.ll"
                writeFile outFile out
                when debug $ putStr out
                exitSuccess
            else do
                putStrLn "Failure, using undeclared variables:"
                putStr $ unlines $ reverse invalidSt
                exitFailure

main = do
    args <- getArgs
    case args of
        "JVM":fileName:tail -> processAndCompile JVM fileName $ not $ null tail
        "LLVM":fileName:tail -> processAndCompile LLVM fileName $ not $ null tail
        _ -> putStrLn "Arguments should be mode (JVM/LLVM), fileName, (optional) debug."
