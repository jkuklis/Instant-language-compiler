module Main where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import AbsInstant
import LexInstant
import SkelInstant
import ParInstant
import ErrM


data MapEntry = MInteger Integer | MString String

fromMInteger :: Integer -> MapEntry -> Integer
fromMInteger def (MInteger rInt) = rInt
fromMInteger def _ = def

fromMString :: String -> MapEntry -> String
fromMString def (MString rStr) = rStr
fromMString def _ = def


compile :: Program -> State (M.Map String MapEntry) String
compile (Prog []) = do
    compiled <- gets $ M.lookup "_compiled"
    let defString = "Error while compiling" 
    return $ fromMString defString $ fromMaybe (MString "") compiled
         
compile (Prog (st:sts)) = do
    case st of
        SAss (Ident id) e -> return "End"
        SExp e -> return "End"
    compile (Prog sts)


main = do
    input <- getContents
    let lexed = myLexer input in case pProgram lexed of
        Bad s -> putStrLn "Bad program"
        Ok p -> do
            let initState = M.insert "_compiled" (MString "a") M.empty
            print $ evalState (compile p) initState
