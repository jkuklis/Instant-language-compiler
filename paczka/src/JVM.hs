{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module JVM where

import Control.Monad.State

import AbsInstant

compileJVM :: Program -> String

compileJVM prog =
    "a"
