{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module LLVM where

import Control.Monad.State

import AbsInstant


data Operation = Add | Sub | Mul | Div


data CodegenState = CodegenState {
    counter :: Integer,
    genCode :: [String]
    } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Monad, MonadState CodegenState )


appendLine :: String -> CodegenState -> CodegenState

appendLine line state = state { genCode = line : (genCode state) }


getCounter :: Codegen Integer

getCounter = do
    count <- gets counter
    modify $ \s -> s { counter = count + 1 }
    return count

        
emitLoad :: String -> Integer -> Codegen ()

emitLoad id counter =
    let loadLine = "%" ++ (show counter) ++ " = load i32, i32* %" ++ id
    in modify $ appendLine loadLine


emitStore :: String -> String -> Codegen ()

emitStore id val =
    let storeLine = "store i32 " ++ val ++ ", i32* %" ++ id
    in modify $ appendLine storeLine


emitPrint :: String -> Codegen ()

emitPrint val =
    let printLine = "call void @printInt(i32 " ++ val ++ ")"
    in modify $ appendLine printLine


emitOp :: String -> String -> Operation -> Codegen String

emitOp v1 v2 op = do
    count <- getCounter
    let 
        oper = case op of
            Add -> "add"
            Sub -> "sub"
            Mul -> "mul"
            Div -> "sdiv"
    let operLine = "%" ++ (show count) ++ " = " ++ oper ++ " i32 " ++ v1 ++ ", " ++ v2
    modify $ appendLine operLine
    return $ "%" ++ (show count)


allocate :: [String] -> String
allocate locals = unlines $ map (\x -> "\t%" ++ x ++ " = alloca i32") locals


compileOp :: Exp -> Exp -> Operation -> Codegen String

compileOp e1 e2 op = do
    v1 <- compileExp e1
    v2 <- compileExp e2
    emitOp v1 v2 op


compileExp :: Exp -> Codegen String

compileExp e = case e of
    ExpLit int -> return $ show int
    ExpVar (Ident id) -> do
        count <- getCounter
        emitLoad id count
        return $ "%" ++ (show count)
    ExpAdd e1 e2 -> compileOp e1 e2 Add
    ExpSub e1 e2 -> compileOp e1 e2 Sub
    ExpMul e1 e2 -> compileOp e1 e2 Mul
    ExpDiv e1 e2 -> compileOp e1 e2 Div


compile :: Program -> Codegen String

compile (Prog []) = do
    instr <- gets genCode
    return $ unlines $ reverse $ map (\x -> "\t" ++ x) instr

compile (Prog (st:stmts)) = case st of
    SAss (Ident id) e -> do
        retVal <- compileExp e
        emitStore id retVal
        compile $ Prog stmts
    SExp e -> do
        retVal <- compileExp e
        emitPrint retVal
        compile $ Prog stmts


startState = CodegenState {
    counter = 0, 
    genCode = []
}

fileBegin = unlines
    ["declare void @printInt(i32)",
    "define i32 @main() {",
    "entry:"]

fileEnd = unlines
    ["\tret i32 0",
    "}"]


compileLLVM :: Program -> [String] -> String

compileLLVM prog locals =
    let
        alloca = allocate locals
        instr = evalState (runCodegen (compile prog)) startState
    in fileBegin ++ alloca ++ instr ++ fileEnd
