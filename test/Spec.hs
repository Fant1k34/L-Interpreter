import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure, Assertion)

import Data (E(..), X(..), F (Fun), S (ExprAsS))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)
import Parser (Parser(getParserFunc))
import ParserFunc (parserProgram)


launchParser :: [Char] -> F Float -> Assertion
launchParser sourceCode correctValue = do
    let ast = getParserFunc (parserProgram "main" []) sourceCode

    case ast of
        Left comment -> fail $ "Parsing Error: " ++ comment
        Right (_, value) -> value @?= correctValue


mainGroup :: TestTree
mainGroup = testGroup "Parser" [ fromDecimalGroup ]
  where
    fromDecimalGroup = testGroup "Expressions"
      [
        testCase "1.0" $ launchParser "1.0" (Fun "main" [] [] (ExprAsS (Number 1)))
      ]


main :: IO ()
main = defaultMain $ testGroup "Tests" [ mainGroup ]