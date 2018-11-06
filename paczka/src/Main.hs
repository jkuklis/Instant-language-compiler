module Main where

import System.Environment ( getArgs )

import AbsInstant
import ParInstant
import ErrM

import LLVM (compileLLVM)
import JVM (compileJVM)

import LocalsGatherer (getLocals)


data VM = LLVM | JVM


compile :: String -> VM -> IO ()

compile fileName vm = do
    input <- getContents
    let tokens = myLexer input in case pProgram tokens of
        Bad error -> do 
            putStrLn "Failure to parse program!"
            putStrLn error
        Ok prog -> case getLocals prog of
            Right locals -> let 
                out = case vm of
                    LLVM -> compileLLVM prog
                    JVM -> compileJVM prog
                in writeFile fileName out
            Left errors -> do
                putStrLn "Failure, using undeclared variables:"                
                putStr errors


main = do
    args <- getArgs
    case args of
        ["LLVM", fileName] -> compile fileName LLVM
        ["JVM", fileName] -> compile fileName JVM
        _ -> putStrLn "Two arguments expected: [LLVM/JVM] fileName"
