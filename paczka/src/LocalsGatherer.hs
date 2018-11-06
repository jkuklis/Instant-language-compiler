{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module LocalsGatherer where


import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.State

import Data.Maybe
import Data.Either

import AbsInstant


data LocalsState = LocalsState {
    counter :: Integer,
    varsFrequency :: M.Map String Integer,
    errors :: [String]
    } deriving Show


newtype LocalsGen a = LocalsGen { runLocalsGen :: State LocalsState a }
  deriving (Functor, Applicative, Monad, MonadState LocalsState )


startState = LocalsState {
    counter = 0,
    varsFrequency = M.empty,
    errors = []
}


incCounter :: LocalsGen ()

incCounter = modify $ \s -> s { counter = (counter s) + 1 }
    

setFreq :: String -> Integer -> LocalsGen ()

setFreq id freq = modify $ \s -> s { varsFrequency = M.insert id freq (varsFrequency s) }


undeclVars :: Exp -> LocalsGen [String]

undeclVars e =
    let helper e1 e2 = do
        u1 <- undeclVars e1
        u2 <- undeclVars e2
        return (u1 ++ u2) 
    in case e of
    (ExpLit _) -> return []
    (ExpVar (Ident id)) -> do
        freq <- gets $ M.lookup id . varsFrequency
        case freq of
            Just fr -> do
                setFreq id $ fr + 1
                return []
            Nothing -> do
                setFreq id 1
                return [id]
    (ExpAdd e1 e2) -> helper e1 e2
    (ExpSub e1 e2) -> helper e1 e2
    (ExpMul e1 e2) -> helper e1 e2
    (ExpDiv e1 e2) -> helper e1 e2


addError :: [String] -> LocalsGen ()

addError undeclVars = do
    count <- gets counter
    let error = "Line " ++ (show count) ++ ": " ++ (unwords (map (\x -> x ++ " ") undeclVars))
    modify $ \s -> s { errors = error : (errors s) }


getFreqCheckErrors :: Exp -> LocalsGen (M.Map String Integer)

getFreqCheckErrors e = do
    freq <- gets varsFrequency
    undecl <- undeclVars e
    when (not (null undecl)) $ addError undecl
    incCounter
    return freq


gatherLocals :: Program -> LocalsGen ([(String, Integer)], [String])

gatherLocals (Prog []) = do
    frequencies <- gets varsFrequency
    err <- gets errors 
    return (M.toList frequencies, err)

gatherLocals (Prog (st:stmts)) = case st of
    SAss (Ident id) e -> do
        freq <- getFreqCheckErrors e
        case M.lookup id freq of
            Just fr -> setFreq id $ fr + 1
            Nothing -> setFreq id 1
        gatherLocals $ Prog stmts
    SExp e -> do
        getFreqCheckErrors e
        gatherLocals $ Prog stmts
          

getLocals :: Program -> Either String [(String, Integer)]

getLocals prog =
    let (frequencies, errors) = evalState (runLocalsGen (gatherLocals prog)) startState
    in if null errors
        then 
            let 
                sortedVars = map fst $ reverse $ L.sortOn snd frequencies
                enumeration = map toInteger [0..(length sortedVars - 1)]
                locals = zip sortedVars enumeration
            in Right locals
        else Left $ unlines $ reverse $ errors
