{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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


data Struct = Struct {
    a :: Integer,
    b :: Integer
    } deriving Show

newtype St a = St { runSt :: State Struct a }
  deriving (Functor, Applicative, Monad, MonadState Struct )

c :: St Integer
c = do 
    modify $ \s -> s {a = a s + 1}
    return 3

main2 = putStrLn $ show $ runState (runSt c) $ Struct {a = 0, b = 0}


data CodegenState = CodegenState {
    counter :: Integer,
    genCode :: [String]
    } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


compileExpLLVM :: Exp -> Codegen String

compileExpLLVM e = return "a"

appendLine :: String -> CodegenState -> CodegenState

appendLine line state = state {genCode = line : (genCode state)}


emitStore :: String -> String -> Codegen ()

emitStore id val =
    let storeLine = "\tstore i32 " ++ val ++ ", i32* %" ++ id
    in modify $ appendLine storeLine


emitPrint :: String -> Codegen ()

emitPrint val =
    let printLine = "\tcall void @printInt(i32 " ++ val ++ ")"
    in modify $ appendLine printLine

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


main = do
    input <- getContents
    let tokens = myLexer input in case pProgram tokens of
        Bad error -> do 
            putStrLn "Failure to parse program!"
            putStrLn error
            putStrLn "Tokens:"
            putStrLn $ show tokens
            exitFailure
        Ok prog -> putStrLn $ evalState (runCodegen (compileLLVM prog)) $ CodegenState {counter = 0, genCode = []}
