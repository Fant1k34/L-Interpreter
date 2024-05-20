{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data (F(..), E(..), S(..), X(..), Op2(..))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)

main :: IO ()
main = do
    start3
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


start2 :: (Floating a, Show a, Ord a) => IO (Either String (E a))
start2 = do
    let invert = Fun "invert" [(Var "x")] [] (ExprAsS (CE (VarAsExpr (Var "x")) Plus (Str "HELLO!")))
    let getData = Fun "getData" [] [] ReadStr

    let mainM = Fun "main" [] [invert, getData] (ExprAsS (FunCall "invert" [FunCall "getData" []]))

    result <- runIOT $ evalStateT (evalFunc mainM []) [([], [])]
    print result

    return result


start3 :: (Floating a, Show a, Ord a) => IO (Either String (E a))
start3 = do
    let getData = Fun "getData" [] [] (ReadStr)
    let mainM = Fun "main" [] [getData] (While (CE (FunCall "getData" []) NotEql (Str "AAA")) (Seq (Pris (Var "k") (FunCall "getData" [])) (Write $ VarAsExpr (Var "k"))))

    result <- runIOT $ evalStateT (evalFunc mainM []) [([], [(Var "outputFile", Str "example.txt")])]
    print result

    return result
