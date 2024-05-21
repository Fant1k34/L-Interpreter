{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data (F(..), E(..), S(..), X(..), Op2(..))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)
import ParserExpr (parserExpr)
import Parser (Parser(getParserFunc))
import ParserStatement (parserStatement)
import ParserFunc (parserProgram)

main :: IO ()
main = do
    print "<L interpreter>: Chose working mode"
    print "<L interpreter>: Type \"s\" for single execution mode (default)"
    print "<L interpreter>: Type \"i\" for interactive mode"
    mode <- getLine

    if mode == "i" || mode == "i\n" || mode == "I" || mode == "I\n" then (do
        print "<L interpreter>: Sometime later..."
        ) else (do
            print "<L interpreter>: Write file name with source code (default: input.txt)"
            sourceCodeFileName <- getLine

            if sourceCodeFileName == "\n" || sourceCodeFileName == "" then (do
                result <- start "input.txt"

                print result
                ) else (do
                    result <- start sourceCodeFileName

                    print result
                )
            )



start :: String -> IO (Either String (E Float))
start inputFile = do
    sourceCode <- readFile inputFile

    let code = getParserFunc (parserProgram "main" []) sourceCode

    case code of
        Left comment -> return $ Left $ "Parsing Error: " ++ comment
        Right (leftover, value) -> (do
            result <- runIOT $ evalStateT (evalFunc value []) [([], [(Var "outputFile", Str "output.txt")])]

            return result
            )

