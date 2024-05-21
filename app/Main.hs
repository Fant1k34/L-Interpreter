{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data (E(..), X(..))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)
import Parser (Parser(getParserFunc))
import ParserFunc (parserProgram)

main :: IO ()
main = do
    putStrLn "<L interpreter>: Chose working mode"
    putStrLn "<L interpreter>: Type \"s\" for single execution mode (default)"
    putStrLn "<L interpreter>: Type \"i\" for interactive mode"
    mode <- getLine

    if mode == "i" || mode == "i\n" || mode == "I" || mode == "I\n" then interactive else (do
            print "<L interpreter>: Write file name with source code (default: input.txt)"
            sourceCodeFileName <- getLine

            if sourceCodeFileName == "\n" || sourceCodeFileName == "" then (do
                sourceCode <- readFile "input.txt"
                result <- start sourceCode

                print result
                ) else (do
                    sourceCode <- readFile sourceCodeFileName
                    result <- start sourceCode

                    print result
                )
            )


interactive :: IO ()
interactive = do
    putStrLn "<L interpreter>: "
    sourceCode <- getLine

    result <- start sourceCode

    putStrLn $ "<L interpreter>: " ++ show result
    interactive



start :: String -> IO (Either String (E Float))
start sourceCode = do
    let ast = getParserFunc (parserProgram "main" []) sourceCode

    case ast of
        Left comment -> return $ Left $ "Parsing Error: " ++ comment
        Right (_, value) -> (do
            result <- runIOT $ evalStateT (evalFunc value []) [([], [(Var "outputFile", Str "output.txt")])]

            return result
            )
