{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Lib

import Control.Monad.Trans.State.Lazy (evalStateT)
import System.IO.Unsafe
import Control.Monad.Trans.IO
import Data
import Text.Printf (printf)

main :: IO ()
main = do
    start
    putStrLn ""



start :: (Floating a, Show a, Ord a) => IO (Either String (E a))
start = do
    let cond = CE (CE (VarAsExpr (Var "n")) Eql (Number 0)) Or (CE (VarAsExpr (Var "n")) Eql (Number 1))
    let pos = ExprAsS (Number 1)
    let neg = ExprAsS (CE (FunCall "fib" [CE (VarAsExpr (Var "n")) Min (Number 1)]) Plus (FunCall "fib" [CE (VarAsExpr (Var "n")) Min (Number 2)]))

    let fib = Fun "fib" [Var "n"] [] (Seq (Write $ VarAsExpr (Var "n")) (Seq (Write $ Str "\n") (If cond pos neg)))
    let mainL = Fun "main" [] [fib] (ExprAsS (FunCall "fib" [Number 25]))


    result <- runIOT $ evalStateT (evalFunc mainL []) [([], [])]
    print result

    return result
