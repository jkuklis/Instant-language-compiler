{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module LLVM where

import Control.Monad.State

import AbsInstant

data CodegenState = CodegenState {
    counter :: Integer,
    genCode :: [String]
    } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


appendLines :: [String] -> CodegenState -> CodegenState

appendLines lines state = state { genCode = lines ++ (genCode state) }


appendLine :: String -> CodegenState -> CodegenState

appendLine line = appendLines [line]


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


emitOp :: Exp -> Exp -> String -> Codegen String

emitOp e1 e2 oper = do
    v1 <- compileExp e1
    v2 <- compileExp e2
    count <- getCounter
    let operLine = "%" ++ (show count) ++ " = " ++ oper ++ " i32 " ++ v1 ++ ", " ++ v2
    modify $ appendLine operLine
    return $ "%" ++ (show count)


compileExp :: Exp -> Codegen String

compileExp e = case e of
    ExpLit int -> return $ show int
    ExpVar (Ident id) -> do
        count <- getCounter
        emitLoad id count
        return $ "%" ++ (show count)
    ExpAdd e1 e2 -> emitOp e1 e2 "add"
    ExpSub e1 e2 -> emitOp e1 e2 "sub"
    ExpMul e1 e2 -> emitOp e1 e2 "mul"
    ExpDiv e1 e2 -> emitOp e1 e2 "sdiv"


compile :: Program -> Codegen String

compile (Prog []) = do
    compiledProg <- gets genCode
    return $ unlines $ reverse $ map (\x -> "\t" ++ x) compiledProg

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


compileLLVM :: Program -> String

compileLLVM prog =
    let instr = evalState (runCodegen (compile prog)) startState
    in fileBegin ++ instr ++ fileEnd
