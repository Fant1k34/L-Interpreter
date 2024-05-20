{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data (F(..), E(..), S(..), X(..), Op2(..))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)
import ParserExpr (expressionParserL0)
import Parser (Parser(getParserFunc))

main :: IO ()
main = do
    start4
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


start4 :: IO (Either String (E Float))
start4 = do
    let structure = getParserFunc expressionParserL0 "3 * 4 + 5 * 6 - 10 / 2"
    let structure2 = getParserFunc expressionParserL0 "1 == 2 || 3 * 5 != 20 && 5 + 7 <= 12"

    case structure2 of
        Left comment -> (do
            let mainM = Fun "main" [] [] (While (CE (FunCall "getData" []) NotEql (Str "AAA")) (Seq (Pris (Var "k") (FunCall "getData" [])) (Write $ VarAsExpr (Var "k"))))

            result <- runIOT $ evalStateT (evalFunc mainM []) [([], [(Var "outputFile", Str "example.txt")])]
            print result

            return result
            )
        Right (_, value) -> (do
            let mainM = Fun "main" [] [] (ExprAsS value)

            result <- runIOT $ evalStateT (evalFunc mainM []) [([], [(Var "outputFile", Str "example.txt")])]
            print result

            return result
            )
    
