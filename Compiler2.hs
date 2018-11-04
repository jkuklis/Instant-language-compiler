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
import LexInstant
import SkelInstant
import ParInstant
import ErrM


type Exception = String



invalidVars_h :: Exp -> Exp -> M.Map String Integer -> S.Set String

invalidVars_h e1 e2 m = S.union (invalidVars e1 m) $ invalidVars e2 m


invalidVars :: Exp -> M.Map String Integer -> S.Set String

invalidVars (ExpLit _) m = S.empty
invalidVars (ExpVar (Ident id)) m = case M.lookup id m of
    Nothing -> S.singleton id
    _ -> S.empty
invalidVars (ExpAdd e1 e2) m = invalidVars_h e1 e2 m
invalidVars (ExpSub e1 e2) m = invalidVars_h e1 e2 m
invalidVars (ExpMul e1 e2) m = invalidVars_h e1 e2 m
invalidVars (ExpDiv e1 e2) m = invalidVars_h e1 e2 m


getVariables :: Program -> Integer -> (M.Map String Integer, [String]) -> (M.Map String Integer, [String])

getVariables (Prog []) _ res = res
getVariables (Prog (st:stmts)) count (m, invalidSt) = 
    let errorLines var st stmts = ("Undeclared variables: " ++ (unwords (map (\x -> x ++ ",") (S.toList var))) ++ " in line " ++ (show count)) : stmts
    in case st of
    SAss (Ident id) e ->
        let
            invalidV = invalidVars e m 
            pos = M.lookup id m
            m' = case pos of
                Nothing -> M.insert id (toInteger (M.size m)) m
                _ -> m
            invalidSt' = if S.null invalidV
                then invalidSt
                else errorLines invalidV st invalidSt
        in getVariables (Prog stmts) (count + 1) (m', invalidSt')
    SExp e ->
        let
            invalidV = invalidVars e m
            invalidSt' = if S.null invalidV
                then invalidSt
                else errorLines invalidV st invalidSt
        in getVariables (Prog stmts) (count + 1) (m, invalidSt')


depth_h :: Exp -> Exp -> Integer
depth_h e1 e2 = 
    let
        d1 = depth e1
        d2 = depth e2
        inc = if d1 == d2
            then 1
            else 0
    in (max d1 d2) + inc


depth :: Exp -> Integer
depth (ExpLit _) = 1
depth (ExpVar _) = 1
depth (ExpAdd e1 e2) = depth_h e1 e2
depth (ExpSub e1 e2) = depth_h e1 e2
depth (ExpMul e1 e2) = depth_h e1 e2
depth (ExpDiv e1 e2) = depth_h e1 e2


calcMaxDepth :: Program -> Integer

calcMaxDepth (Prog stmts) = foldl max 0 $ map (\st -> case st of
    SAss _ e -> depth e
    SExp e -> depth e) stmts





createFileJVM :: Program -> (M.Map String Integer) -> IO ()

createFileJVM p locals =
    let
        initCode =
            [".class  public Hello",
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
        stackLine = ".stack limit " ++ (show stackLimit)
        instrCode = []
        jasminOut = unlines $ initCode ++ [localsLine, stackLine] ++ (reverse instrCode) ++ endCode
    in do
        writeFile "out.j" jasminOut  
        putStr jasminOut

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
        Ok prog -> do
            let 
                (locals, invalidV) = getVariables prog 0 (M.empty, [])
            putStr $ unlines $ reverse invalidV
        --    createFileLLVM prog
            createFileJVM prog locals


main = processAndCompile
