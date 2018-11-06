module Main where

import System.Environment ( getArgs )

import AbsInstant
import ParInstant
import ErrM

import LLVM (compileLLVM)
import JVM (compileJVM)

import LocalsGatherer (getLocals)


data VM = LLVM | JVM


compile :: String -> String -> VM -> IO ()

compile fileDir fileName vm = do
    input <- getContents
    let tokens = myLexer input in case pProgram tokens of
        Bad error -> do 
            putStrLn "Failure to parse program!"
            putStrLn error
        Ok prog -> case getLocals prog of
            Right locals -> let 
                out = case vm of
                    LLVM -> compileLLVM prog locals
                    JVM -> compileJVM prog locals fileName
                ext = case vm of
                    LLVM -> ".ll"
                    JVM -> ".j"
                in writeFile (fileDir ++ "/" ++ fileName ++ ext) out
            Left errors -> do
                putStrLn "Failure, using undeclared variables:"                
                putStr errors

main = do
    args <- getArgs
    case args of
        ["LLVM", fileDir, fileName] -> compile fileDir fileName LLVM
        ["JVM", fileDir, fileName] -> compile fileDir fileName JVM
        _ -> putStrLn "Three arguments expected: [LLVM/JVM] fileDir fileName"
