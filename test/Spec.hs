import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure, Assertion)

import Data (E(..), X(..), F (Fun), S (..), Op2(..))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)
import Parser (Parser(getParserFunc))
import ParserFunc (parserProgram)

import System.IO.Unsafe


launchParser :: [Char] -> F Float -> Assertion
launchParser sourceCode correctValue = do
    let ast = getParserFunc (parserProgram "main" []) sourceCode

    case ast of
        Left comment -> fail $ "Parsing Error: " ++ comment
        Right (_, value) -> value @?= correctValue


parserGroup :: TestTree
parserGroup = testGroup "Parser" [ expressionsGroup, statementsGroup, functionsGroup ]
  where
    expressionsGroup = testGroup "Expressions"
      [
        testCase "1.0" $ launchParser "1.0" (Fun "main" [] [] (ExprAsS (Number 1))),
        testCase "  0.15" $ launchParser "  0.15" (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "  0.15  " $ launchParser "  0.15  " (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "0.15  " $ launchParser "0.15  " (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "0.15\t " $ launchParser "0.15\t " (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "31.02*78984.15" $ launchParser "31.02*78984.15" (Fun "main" [] [] (ExprAsS (CE (Number 31.02) Mult (Number 78984.15)))),
        testCase "2.0 + 95.84" $ launchParser "2.0 + 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Plus (Number 95.84)))),
        testCase "2.0 - 95.84" $ launchParser "2.0 - 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Min (Number 95.84)))),
        testCase "2.0 * 95.84" $ launchParser "2.0 * 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Mult (Number 95.84)))),
        testCase "2.0 / 95.84" $ launchParser "2.0 / 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Del (Number 95.84)))),
        testCase "x1 + y1 - 55.04" $ launchParser "x1 + y1 - 55.04" (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Plus (VarAsExpr (Var "y1"))) Min (Number 55.04))),
        testCase "x1 - y1 - 55.04" $ launchParser "x1 - y1 - 55.04" (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Min (VarAsExpr (Var "y1"))) Min (Number 55.04))),
        testCase "\"Example of String\"" $ launchParser "\"Example of String\"" (Fun "main" [] [] (ExprAsS (Str "Example of String"))),
        testCase "\"Example of String\" + \"!\" + \"??\"" $ launchParser "\"Example of String\" + \"!\" + \"??\""
         (Fun "main" [] [] (ExprAsS $ CE (CE (Str "Example of String") Plus (Str "!")) Plus (Str "??"))),
        testCase "True" $ launchParser "True" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "False" $ launchParser "False" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "False || False" $ launchParser "False || False" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "False || True" $ launchParser "False || True" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "True || False" $ launchParser "True || False" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "True || True" $ launchParser "True || True" (Fun "main" [] [] (ExprAsS $ CE (Boolean True) Or (Boolean True))),
        testCase "False && False" $ launchParser "False && False" (Fun "main" [] [] (ExprAsS $ CE (Boolean False) And (Boolean False))),
        testCase "False && True" $ launchParser "False && True" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "True && False" $ launchParser "True && False" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "True && True" $ launchParser "True && True" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "True && False || False && True && False || True || False" $
         launchParser "True && False || False && True && False || True || False"
          (Fun "main" [] [] (ExprAsS $ CE (CE (Boolean False) And (Boolean False)) Or (Boolean True))),
        testCase "True && someExpression" $ launchParser "True && someExpression" (Fun "main" [] [] (ExprAsS (VarAsExpr (Var "someExpression")))),
        testCase "True || someExpression" $ launchParser "True || someExpression" (Fun "main" [] [] (ExprAsS $ CE (Boolean True) Or (VarAsExpr (Var "someExpression")))),
        testCase "False && someExpression" $ launchParser "False && someExpression" (Fun "main" [] [] (ExprAsS $ CE (Boolean False) And (VarAsExpr (Var "someExpression")))),
        testCase "False || someExpression" $ launchParser "False || someExpression" (Fun "main" [] [] (ExprAsS (VarAsExpr (Var "someExpression")))),
        testCase "1.0 == 2.0" $ launchParser "1.0 == 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) Eql (Number 2))),
        testCase "1.0 != 2.0" $ launchParser "1.0 != 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) NotEql (Number 2))),
        testCase "1.0 < 2.0" $ launchParser "1.0 < 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) Less (Number 2))),
        testCase "1.0 <= 2.0" $ launchParser "1.0 <= 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) LessEql (Number 2))),
        testCase "1.0 > 2.0" $ launchParser "1.0 > 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) More (Number 2))),
        testCase "1.0 >= 2.0" $ launchParser "1.0 >= 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) MoreEql (Number 2))),
        testCase "1.0 + x < x * x" $ launchParser "1.0 + x < x * x" 
         (Fun "main" [] [] (ExprAsS $ CE (CE (Number 1) Plus (VarAsExpr (Var "x"))) Less (CE (VarAsExpr (Var "x")) Mult (VarAsExpr (Var "x"))))),
        testCase "a < b || b >= c && d == 3" $ launchParser "a < b || b >= c && d == 3"
         (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "a")) Less (VarAsExpr (Var "b"))) Or
          (CE (CE (VarAsExpr (Var "b")) MoreEql (VarAsExpr (Var "c"))) And (CE (VarAsExpr (Var "d")) Eql (Number 3)))  ))
      ]
    statementsGroup = testGroup "Statements" 
      [
        testCase "if b >= c && d == 3 then { skip } else { skip }" 
          $ launchParser "if b >= c && d == 3 then { skip } else { skip }" 
            (Fun "main" [] [] (If (CE (CE (VarAsExpr (Var "b")) MoreEql (VarAsExpr (Var "c"))) And (CE (VarAsExpr (Var "d")) Eql (Number 3))) (Skip) (Skip))),

        testCase "if cond then { 3 } else { 4 }" 
          $ launchParser "if cond then { 3 } else { 4 }" 
            (Fun "main" [] [] (If (VarAsExpr (Var "cond")) (ExprAsS $ Number 3) (ExprAsS $ Number 4))),
        
        testCase "if cond then { 3 } else { 4 } WITH SEPARATORS" 
          $ launchParser "if\n\n cond\n \nthen\n{3} \t\n\telse {\n4 }\n\t" 
            (Fun "main" [] [] (If (VarAsExpr (Var "cond")) (ExprAsS $ Number 3) (ExprAsS $ Number 4))),
        
        testCase "while b >= c && d == 3 do { 3 }" 
          $ launchParser "while b >= c && d == 3 do { 3 }" 
            (Fun "main" [] [] (While (CE (CE (VarAsExpr (Var "b")) MoreEql (VarAsExpr (Var "c"))) And (CE (VarAsExpr (Var "d")) Eql (Number 3))) (ExprAsS $ Number 3))),

        testCase "while cond do { 3 }" 
          $ launchParser "while cond do { 3 }" 
            (Fun "main" [] [] (While (VarAsExpr (Var "cond")) (ExprAsS $ Number 3))),
        
        testCase "while cond do { 3 } WITH SEPARATOR" 
          $ launchParser "while \n\tcond \tdo\n\n\n\n { \n\t\t3}" 
            (Fun "main" [] [] (While (VarAsExpr (Var "cond")) (ExprAsS $ Number 3))),
        
        testCase "Nested IF and WHILE" 
          $ launchParser "if cond then { while cond do { 3 } } else { if cond then { while cond do { 3 } } else { \"Мяу\" } }" 
            (Fun "main" [] [] (If (VarAsExpr (Var "cond")) (While (VarAsExpr (Var "cond")) (ExprAsS $ Number 3)) (If (VarAsExpr (Var "cond")) (While (VarAsExpr (Var "cond")) (ExprAsS $ Number 3)) (ExprAsS (Str "Мяу"))))),

        testCase "write \"Result \" + \"VALUE\"" 
          $ launchParser "write \"Result \" + \"VALUE\"" 
            (Fun "main" [] [] (Write $ CE (Str "Result ") Plus (Str "VALUE"))),
        
        testCase "write \"Result \" + \"VALUE\" WITH SEPARATOR" 
          $ launchParser "write   \t\"Result \" + \"VALUE\"" 
            (Fun "main" [] [] (Write $ CE (Str "Result ") Plus (Str "VALUE"))),

        testCase "read" 
          $ launchParser "read" 
            (Fun "main" [] [] ReadStr),
        
        testCase "sequence of statements" 
          $ launchParser "read\nread\nwrite \"Something\"\n" 
            (Fun "main" [] [] $ Seq (Seq (Seq (Seq Skip Skip) ReadStr) ReadStr) (Write $ Str "Something")),

        testCase "expr := 58 * 412.5" 
          $ launchParser "expr := 58 * 412.5" 
            (Fun "main" [] [] $ Pris (Var "expr") (CE (Number 58) Mult (Number 412.5))),
        
        testCase "expr := x" 
          $ launchParser "expr := x" 
            (Fun "main" [] [] $ Pris (Var "expr") (VarAsExpr (Var "x"))),
        
        testCase "combination"
          $ launchParser "getInput n <- { read }  input := getInput(\"\", 4, 15) if input == \"test\" || input != \"prod\" && input != \"\" then { output := 5 * input read write \"Okej\" } else { \"Combine all together\" while True do { if False then { read } else { write \"Okej\" } read read output := input getInput(\"\") } } read write \"Okej\"" 
            (Fun "main" [] [Fun "getInput" [Var "n"] [] ReadStr] $ (Seq (Seq (Seq (Seq (Seq Skip Skip) (Pris (Var "input") (FunCall "getInput" [Str "",Number 4.0,Number 15.0]))) (If (CE (CE (VarAsExpr $ Var "input") Eql (Str "test")) Or (CE (CE (VarAsExpr $ Var "input") NotEql (Str "prod")) And (CE (VarAsExpr $ Var "input") NotEql (Str "")))) (Seq (Seq (Seq (Seq Skip Skip) (Pris (Var "output") (CE (Number 5.0) Mult (VarAsExpr (Var "input"))))) ReadStr) (Write (Str "Okej"))) (Seq (Seq (Seq Skip Skip) (ExprAsS (Str "Combine all together"))) (While (Boolean True) (Seq (Seq (Seq (Seq (Seq (Seq Skip Skip) (If (Boolean False) ReadStr (Write (Str "Okej")))) ReadStr) ReadStr) (Pris (Var "output") (VarAsExpr (Var "input")))) (ExprAsS (FunCall "getInput" [Str ""]))))))) ReadStr) (Write (Str "Okej"))))
      ]
    functionsGroup = testGroup "Functions" 
      [
        testCase "function with 1 arg"
          $ launchParser "getInput n <- { read } input := getInput(\"\")"
            (Fun "main" [] [Fun "getInput" [Var "n"] [] ReadStr] (Pris (Var "input") (FunCall "getInput" [Str ""]))),
        
        testCase "function with 2 arg"
          $ launchParser "getInput n m <- { read } input := getInput(\"\", 5)"
            (Fun "main" [] [Fun "getInput" [Var "n", Var "m"] [] ReadStr] (Pris (Var "input") (FunCall "getInput" [Str "", Number 5.0]))),
        
        testCase "function with 3 arg"
          $ launchParser "getInput n m k <- { read } input := getInput(\"\", x, 5.854)"
            (Fun "main" [] [Fun "getInput" [Var "n", Var "m", Var "k"] [] ReadStr] (Pris (Var "input") (FunCall "getInput" [Str "", VarAsExpr (Var "x"), Number 5.854]))),
        
        testCase "function with 4 arg"
          $ launchParser "getInput n m k r <- { read } input := getInput(\"\", x, 5.854, True)"
            (Fun "main" [] [Fun "getInput" [Var "n", Var "m", Var "k", Var "r"] [] ReadStr] (Pris (Var "input") (FunCall "getInput" [Str "", VarAsExpr (Var "x"), Number 5.854, Boolean True]))),

        testCase "inner functions"
          $ launchParser "getInput n <- { anotherFunc a b <- { a + b } oneMoreFunc x y <- { x * y write x * y read anotherFunc(x, y) } oneMoreFunc(4, 5) anotherFunc(2, 3) read } input := getInput(\"\")"
            (Fun "main" [] [Fun "getInput" [Var "n"] [Fun "anotherFunc" [Var "a", Var "b"] [] (ExprAsS (CE (VarAsExpr $ Var "a") Plus (VarAsExpr $ Var "b"))), Fun "oneMoreFunc" [Var "x", Var "y"] [] (Seq 
            (Seq (Seq (Seq (Seq Skip Skip) (ExprAsS (CE (VarAsExpr $ Var "x") Mult (VarAsExpr $ Var "y")))) (Write (CE (VarAsExpr $ Var "x") Mult (VarAsExpr $ Var "y")))) ReadStr) (ExprAsS (FunCall "anotherFunc" [VarAsExpr $ Var "x", VarAsExpr $ Var "y"])))] (Seq (Seq (Seq (Seq Skip Skip) (ExprAsS (FunCall "oneMoreFunc" [Number 4.0, Number 5.0]))) (ExprAsS (FunCall "anotherFunc" 
            [Number 2.0,Number 3.0]))) ReadStr)] (Pris (Var "input") (FunCall "getInput" [Str ""])))
      ]


launchEval :: F Float -> [(X, E Float)] -> IO (Either String (E Float))
launchEval ast varList = do
    result <- runIOT $ evalStateT (evalFunc ast []) [([], [(Var "outputFile", Str "str.out")] ++ varList)]

    return result


evalGroup = testGroup "Eval" [ expressionsGroup, statementsGroup, functionsGroup ]
  where
    expressionsGroup = testGroup "Expressions" 
      [
        testCase "1.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (Number 1))) []) @?= Right (Number 1),
        testCase "0.15" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (Number 0.15))) []) @?= Right (Number 0.15),
        testCase "2.0 + 95.84" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (CE (Number 2) Plus (Number 95.84)))) []) @?= Right (Number 97.84),
        testCase "2.0 * 95.84" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (CE (Number 2) Mult (Number 95.84)))) []) @?= Right (Number 191.68),
        testCase "2.0 - 95.84" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (CE (Number 2) Min (Number 95.84)))) []) @?= Right (Number $ -93.84),
        testCase "80.82 / -6" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (CE (Number 80.82) Del (Number $ -6)))) []) @?= Right (Number $ -13.47),
        testCase "0 / 6" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (CE (Number 0) Del (Number $ 6)))) []) @?= Right (Number $ 0),
        testCase "80.82 / 0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (CE (Number 80.82) Del (Number $ 0)))) []) @?= Right (Number $ 1 / 0),
        testCase "x1 + y1 - 55.04 WITH x1, y1" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Plus (VarAsExpr (Var "y1"))) Min (Number 55.04))) [(Var "x1", Number $ -5), (Var "y1", Number 49.2)]) @?= Right (Number $ -10.84),
        testCase "x1 + y1 - 55.04 WITH x1" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Plus (VarAsExpr (Var "y1"))) Min (Number 55.04))) [(Var "x1", Number $ -5)]) @?= Left "Variable y1 does not exist in context",
        testCase "x1 + y1 - 55.04 WITH y1" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Plus (VarAsExpr (Var "y1"))) Min (Number 55.04))) [(Var "y1", Number 49.2)]) @?= Left "Variable x1 does not exist in context",
        testCase "x1 + y1 - 55.04 WITHOUT x1, y1" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Plus (VarAsExpr (Var "y1"))) Min (Number 55.04))) []) @?= Left "Variable x1 does not exist in context",
        testCase "x1 - y1 - 48 WITH x1, y1" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Min (VarAsExpr (Var "y1"))) Min (Number 48))) [(Var "x1", Number $ 35), (Var "y1", Number 8)]) @?= Right (Number $ -21),
        testCase "string concat" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (Str "Example of String") Plus (Str "!")) Plus (Str "??"))) []) @?= Right (Str "Example of String!??"),

        testCase "True" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (Boolean True))) []) @?= Right (Boolean True),
        testCase "False" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS (Boolean False))) []) @?= Right (Boolean False),
        testCase "False || False" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean False) Or (Boolean False))) []) @?= Right (Boolean False),
        testCase "False || True" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean False) Or (Boolean True))) []) @?= Right (Boolean True),
        testCase "True || False" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean True) Or (Boolean False))) []) @?= Right (Boolean True),
        testCase "True || True" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean True) Or (Boolean True))) []) @?= Right (Boolean True),
        testCase "False && False" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean False) And (Boolean False))) []) @?= Right (Boolean False),
        testCase "False && True" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean False) And (Boolean True))) []) @?= Right (Boolean False),
        testCase "True && False" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean True) And (Boolean False))) []) @?= Right (Boolean False),
        testCase "True && True" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Boolean True) And (Boolean True))) []) @?= Right (Boolean True),
        testCase "False && False || True" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (CE (Boolean False) And (Boolean False)) Or (Boolean True))) []) @?= Right (Boolean True),
        testCase "1.0 == 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 1) Eql (Number 2))) []) @?= Right (Boolean False),
        testCase "1.0 != 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 1) NotEql (Number 2))) []) @?= Right (Boolean True),
        testCase "1.0 < 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 1) Less (Number 2))) []) @?= Right (Boolean True),
        testCase "3.0 < 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 3) Less (Number 2))) []) @?= Right (Boolean False),
        testCase "1.0 <= 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 1) LessEql (Number 2))) []) @?= Right (Boolean True),
        testCase "3.0 <= 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 3) LessEql (Number 2))) []) @?= Right (Boolean False),
        testCase "1.0 > 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 1) More (Number 2))) []) @?= Right (Boolean False),
        testCase "3.0 > 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 3) More (Number 2))) []) @?= Right (Boolean True),
        testCase "1.0 >= 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 1) MoreEql (Number 2))) []) @?= Right (Boolean False),
        testCase "3.0 >= 2.0" $ unsafePerformIO (launchEval (Fun "main" [] [] (ExprAsS $ CE (Number 3) MoreEql (Number 2))) []) @?= Right (Boolean True)
      ]
    statementsGroup = testGroup "Statements" 
      [
        testCase "if b >= c && b == 3 then { 2 * 3 } else { 0 }" $ unsafePerformIO (launchEval (Fun "main" [] [] (If (CE (CE (VarAsExpr (Var "b")) MoreEql (VarAsExpr (Var "c"))) And (CE (VarAsExpr (Var "b")) Eql (Number 3))) (ExprAsS $ CE (Number 2) Mult (Number 3)) (ExprAsS $ Number 0))) [(Var "b", Number 3), (Var "c", Number 1)]) @?= Right (Number 6),
        testCase "if b >= c && b == 3 then { 2 * 3 } else { 0 }" $ unsafePerformIO (launchEval (Fun "main" [] [] (If (CE (CE (VarAsExpr (Var "b")) MoreEql (VarAsExpr (Var "c"))) And (CE (VarAsExpr (Var "b")) Eql (Number 3))) (ExprAsS $ CE (Number 2) Mult (Number 3)) (ExprAsS $ Number 0))) [(Var "b", Number 1), (Var "c", Number 1)]) @?= Right (Number 0),
        testCase "expr := 58 * 412.5; expr" $ unsafePerformIO (launchEval (Fun "main" [] [] $ Seq (Seq (Skip) (Pris (Var "expr") (CE (Number 58) Mult (Number 412.5)))) (ExprAsS (VarAsExpr (Var "expr")))) []) @?= Right (Number 23925),
        testCase "expr := 58 * 412.5; expr := 5 * 15; expr" $ unsafePerformIO (launchEval (Fun "main" [] [] $ Seq (Seq (Seq (Skip) (Pris (Var "expr") (CE (Number 58) Mult (Number 412.5)))) (Pris (Var "expr") (CE (Number 5) Mult (Number 15)))) (ExprAsS (VarAsExpr (Var "expr")))) []) @?= Right (Number (15 * 5)),
        testCase "skip" $ unsafePerformIO (launchEval (Fun "main" [] [] $ Skip) []) @?= Right (Boolean True),
        testCase "skip skip" $ unsafePerformIO (launchEval (Fun "main" [] [] $ Seq Skip Skip) []) @?= Right (Boolean True),
        testCase "i := 0 while i < 5 do { i := i + 1 } i" $ unsafePerformIO (launchEval (Fun "main" [] [] (Seq (Seq (Seq (Seq Skip Skip) (Pris (Var "i") (Number 0.0))) (While (CE (VarAsExpr (Var "i")) Less (Number 5.0)) (Pris (Var "i") (CE (VarAsExpr (Var "i")) Plus (Number 1.0))))) (ExprAsS (VarAsExpr (Var "i"))))) []) @?= Right (Number 5),
        testCase "i := 0 while i > 5 do { i := i + 1 } i" $ unsafePerformIO (launchEval (Fun "main" [] [] (Seq (Seq (Seq (Seq Skip Skip) (Pris (Var "i") (Number 0.0))) (While (CE (VarAsExpr (Var "i")) More (Number 5.0)) (Pris (Var "i") (CE (VarAsExpr (Var "i")) Plus (Number 1.0))))) (ExprAsS (VarAsExpr (Var "i"))))) []) @?= Right (Number 0)
      ]
    functionsGroup = testGroup "Functions"
      [
        testCase "successor" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "successor" [Var "n"] [] (ExprAsS (CE (VarAsExpr $ Var "n") Plus (Number 1.0)))] (ExprAsS (FunCall "successor" [Number 3.0]))) []) @?= Right (Number 4),
        testCase "try access var inside func" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "successor" [Var "n"] [] (ExprAsS (CE (VarAsExpr $ Var "n") Plus (Number 1.0)))] (ExprAsS (VarAsExpr (Var "n")))) []) @?= Left "Variable n does not exist in context",
        testCase "fun with 2 args" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "successorOfSum" [Var "n", Var "m"] [] (ExprAsS (CE (CE (VarAsExpr $ Var "n") Plus (VarAsExpr $ Var "m")) Plus (Number 1.0)))] (ExprAsS (FunCall "successorOfSum" [Number 3.0, Number 5.0]))) []) @?= Right (Number 9),
        testCase "fun with if inside (True)" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "successorIfNot0" [Var "n"] [] (If (CE (VarAsExpr $ Var "n") NotEql (Number 0.0)) (ExprAsS (CE (VarAsExpr $ Var "n") Plus (Number 1.0))) (ExprAsS (VarAsExpr $ Var "n")))] (ExprAsS (FunCall "successorIfNot0" [Number 5.0]))) []) @?= Right (Number 6),
        testCase "fun with if inside (False)" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "successorIfNot0" [Var "n"] [] (If (CE (VarAsExpr $ Var "n") NotEql (Number 0.0)) (ExprAsS (CE (VarAsExpr $ Var "n") Plus (Number 1.0))) (ExprAsS (VarAsExpr $ Var "n")))] (ExprAsS (FunCall "successorIfNot0" [Number 0]))) []) @?= Right (Number 0),
        testCase "complexLogin inside (always True)" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "complexLogin" [Var "n"] [] (Seq (Seq (Seq Skip Skip) (Write (Str "To complex to understand"))) (If (Boolean True) (Seq (Seq (Seq (Seq Skip Skip) (Pris (Var "i") (Number 0.0))) (While (CE (VarAsExpr $ Var "i") Less (Number 5.0)) (Pris (Var "i") (CE (VarAsExpr $ Var "i") Plus (Number 1.0))))) (ExprAsS (VarAsExpr $ Var "i"))) (ExprAsS (Boolean False))))] (ExprAsS (FunCall "complexLogin" [Number 5.0]))) []) @?= Right (Number 5),
        testCase "complexLogin inside (always False)" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "complexLogin" [Var "n"] [] (Seq (Seq (Seq Skip Skip) (Write (Str "To complex to understand"))) (If (Boolean False) (Seq (Seq (Seq (Seq Skip Skip) (Pris (Var "i") (Number 0.0))) (While (CE (VarAsExpr $ Var "i") Less (Number 5.0)) (Pris (Var "i") (CE (VarAsExpr $ Var "i") Plus (Number 1.0))))) (ExprAsS (VarAsExpr $ Var "i"))) (ExprAsS (Boolean False))))] (ExprAsS (FunCall "complexLogin" [Number 5.0]))) []) @?= Right (Boolean False),
        testCase "recursive func" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "successor" [Var "n"] [] (ExprAsS (CE (VarAsExpr $ Var "n") Plus (Number 1.0)))] (ExprAsS (FunCall "successor" [Number 3.0]))) []) @?= Right (Number 4),
        testCase "inner functions" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "getInput" [Var "n"] [Fun "anotherFunc" [Var "a", Var "b"] [] (ExprAsS (CE (VarAsExpr $ Var "a") Plus (VarAsExpr $ Var "b"))), Fun "oneMoreFunc" [Var "x", Var "y"] [] (Seq 
            (Seq (Seq (Seq (Seq Skip Skip) (ExprAsS (CE (VarAsExpr $ Var "x") Mult (VarAsExpr $ Var "y")))) (Write (CE (VarAsExpr $ Var "x") Mult (VarAsExpr $ Var "y")))) (Write $ Str "Anything")) (ExprAsS (FunCall "anotherFunc" [VarAsExpr $ Var "x", VarAsExpr $ Var "y"])))] (Seq (Seq (Seq Skip Skip) (ExprAsS (FunCall "oneMoreFunc" [Number 4.0, Number 5.0]))) (ExprAsS (FunCall "anotherFunc" 
            [Number 2.0,Number 3.0])))] (ExprAsS (FunCall "getInput" [Str ""]))) []) @?= Right (Number 5),
        testCase "try to access inner function outside" $ unsafePerformIO (launchEval (Fun "main" [] [Fun "getInput" [Var "n"] [Fun "anotherFunc" [Var "a", Var "b"] [] (ExprAsS (CE (VarAsExpr $ Var "a") Plus (VarAsExpr $ Var "b"))), Fun "oneMoreFunc" [Var "x", Var "y"] [] (Seq 
            (Seq (Seq (Seq (Seq Skip Skip) (ExprAsS (CE (VarAsExpr $ Var "x") Mult (VarAsExpr $ Var "y")))) (Write (CE (VarAsExpr $ Var "x") Mult (VarAsExpr $ Var "y")))) (Write $ Str "Anything")) (ExprAsS (FunCall "anotherFunc" [VarAsExpr $ Var "x", VarAsExpr $ Var "y"])))] (Seq (Seq (Seq Skip Skip) (ExprAsS (FunCall "oneMoreFunc" [Number 4.0, Number 5.0]))) (ExprAsS (FunCall "anotherFunc" 
            [Number 2.0,Number 3.0])))] (ExprAsS (FunCall "anotherFunc" [Number 4, Number 5]))) []) @?= Left "Function anotherFunc does not exist in context"
      ]


main :: IO ()
main = defaultMain $ testGroup "Tests" [ parserGroup, evalGroup ]