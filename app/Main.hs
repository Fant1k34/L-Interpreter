{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
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
    let squareL = Fun "square" [(Var "x")] [] (ExprAsS (CE (VarAsExpr (Var "x")) Mult (VarAsExpr (Var "x"))))
    let mainL = Fun "main" [] [squareL] (Write $ Number 522)

    let evaluation = evalFunc mainL [] 

    result <- runIOT $ evalStateT evaluation [([], [])]

    printf (show result)

    return result
    
