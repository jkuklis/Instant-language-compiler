{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module JVM where

import Control.Monad.State

import qualified Data.Map as M

import AbsInstant


data Operation = Add | Sub | Mul | Div


data DExp
    = DExpLit Integer
    | DExpVar String
    | DExpOp DExp DExp Integer Integer Operation
    

data CodegenState = CodegenState {
    genCode :: [String],
    local :: M.Map String Integer,
    maxDepth :: Integer
    } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


appendLine :: String -> CodegenState -> CodegenState

appendLine line state = state { genCode = line : (genCode state) }


emitStore :: String -> Codegen ()

emitStore id = do
    Just pos <- gets $ M.lookup id . local
    let storeLine = "istore" ++ (if pos <= 3 then "_" else " ") ++ (show pos)
    modify $ appendLine storeLine


emitLoad :: String -> Codegen ()

emitLoad id = do
    Just pos <- gets $ M.lookup id . local
    let loadLine = "iload" ++ (if pos <= 3 then "_" else " ") ++ (show pos)
    modify $ appendLine loadLine


emitStream :: Codegen ()

emitStream = modify $ appendLine "getstatic java/lang/System/out Ljava/io/PrintStream;"


emitPrint :: Codegen ()

emitPrint = modify $ appendLine "invokevirtual java/io/PrintStream/println(I)V"


emitSwap :: Codegen ()

emitSwap = modify $ appendLine "swap"


emitInt :: Integer -> Codegen ()

emitInt int =
    let 
        line
            | int == -1 = "iconst_m1"
            | 0 <= int && int <= 5 = "iconst_" ++ (show int)
            | -128 <= int && int <= 127 = "bipush " ++ (show int)
            | -32768 <= int && int <= 32767 = "sipush " ++ (show int)
            | otherwise = "ldc " ++ (show int)
    in modify $ appendLine line


emitOp :: Operation -> Codegen ()

emitOp op =
    let 
        line = case op of
            Add -> "iadd"
            Sub -> "isub"
            Mul -> "imul"
            Div -> "idiv"
    in modify $ appendLine line


tryIncreaseDepth :: Integer -> Codegen ()
    
tryIncreaseDepth d = modify $ \s -> s { maxDepth = max d (maxDepth s) }


depthExp :: Exp -> (DExp, Integer)

depthExp e = let 
    helper e1 e2 op =
        let
            (de1, d1) = depthExp e1
            (de2, d2) = depthExp e2
            d = (max d1 d2) + (if d1 == d2 then 1 else 0)
        in (DExpOp de1 de2 d1 d2 op, d)
    in case e of
    ExpLit int -> (DExpLit int, 1)
    ExpVar (Ident id) -> (DExpVar id, 1)
    ExpAdd e1 e2 -> helper e1 e2 Add
    ExpSub e1 e2 -> helper e1 e2 Sub
    ExpMul e1 e2 -> helper e1 e2 Mul
    ExpDiv e1 e2 -> helper e1 e2 Div


compileDExp :: DExp -> Codegen ()

compileDExp de = case de of
    DExpLit int -> emitInt int
    DExpVar id -> emitLoad id
    DExpOp de1 de2 d1 d2 op ->
        if d2 > d1
            then do
                compileDExp de2
                compileDExp de1
                case op of
                    Sub -> emitSwap
                    Div -> emitSwap
                    _ -> return ()
                emitOp op
            else do
                compileDExp de1
                compileDExp de2
                emitOp op


compile :: Program -> Codegen (String, Integer)

compile (Prog []) = do
    instr <- gets genCode
    maxD <- gets maxDepth
    let prog = unlines $ reverse $ map (\x -> "\t" ++ x) instr
    return (prog, maxD)

compile (Prog (st:stmts)) = case st of
    SAss (Ident id) e -> do
        let (de, d) = depthExp e        
        compileDExp de
        tryIncreaseDepth d
        emitStore id
        compile $ Prog stmts
    SExp e -> do
        emitStream
        let (de, d) = depthExp e
        compileDExp de
        tryIncreaseDepth d
        emitPrint 
        compile $ Prog stmts


mapToConsequent :: [String] -> M.Map String Integer

mapToConsequent locals =
    let
        enumeration = map toInteger [0..(length locals - 1)]
        enumerated = zip locals enumeration
    in M.fromList enumerated


startState locals = CodegenState {
    genCode = [],
    local = locals,
    maxDepth = 2
}


beginFile = unlines
    [".super java/lang/Object\n",
    "; standard initializer",
    ".method public <init>()V",
    "\taload_0\n",
    "\tinvokenonvirtual java/lang/Object/<init>()V",
    "\treturn",
    ".end method\n",
    ".method public static main([Ljava/lang/String;)V"]


endFile = unlines
    ["\treturn",
    ".end method"]


compileJVM :: Program -> [String] -> String -> String

compileJVM prog locals fileName =
    let
        firstLine = ".class public " ++ fileName ++ "\n"
        localsLine = ".limit locals " ++ (show (length locals)) ++ "\n"
        start = startState $ mapToConsequent locals
        (instr, maxD) = evalState (runCodegen (compile prog)) start
        stackLine = ".limit stack " ++ (show maxD) ++ "\n"
    in firstLine ++ beginFile ++ localsLine ++ stackLine ++ instr ++ endFile
