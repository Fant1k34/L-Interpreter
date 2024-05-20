{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module ParserExpr where

-- import Parser (Parser(..))
-- import ParserCore

-- import Utils (razryad, concatNumbers, castCharToInt)

-- import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
-- import Control.Applicative (Alternative((<|>), empty))

-- -- import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))
-- import Data.Foldable1 (foldlM1)

-- -- import Basement.Floating
-- import GHC.Integer (Integer)
-- import GHC.Real (Integral)

-- takenNames :: [String]
-- takenNames = ["sqrt", "let", "Nikita"]


-- defineActionByZnak :: Char -> Operator2
-- defineActionByZnak znak
--     | znak == '-' = Min
--     | znak == '+' = Plus
--     | znak == '*' = Mul
--     | znak == '/' = Div
--     | znak == '^' = In
--     | otherwise = error "Inner Error: Something was broken during parsing. defineActionByZnak got not action"


-- parseNumberToExpr :: Integral a => Parser (Expr a)
-- parseNumberToExpr = do
--     numberList <- some (castCharToInt <$> (satisfy isNumber))
--     let result = Prelude.foldl1 concatNumbers numberList

--     return (Arg result)


-- parseFracToExpr :: Floating a => Parser (Expr a)
-- parseFracToExpr = do
--     part1List <- some (castCharToInt <$> satisfy isNumber)
--     let part1 = fromInteger (Prelude.foldl1 concatNumbers part1List)

--     satisfy (=='.')

--     part2List <- some (castCharToInt <$> satisfy isNumber)
--     let part2 = fromInteger (Prelude.foldl1 concatNumbers part2List)

--     let result = part1 + part2 / (10 ^ (length part2List))
--     return (Arg result)


-- parseIndentToExpr :: Num a => Parser (Expr a)
-- parseIndentToExpr = do
--     var <- parseIndet

--     if (foldl1 (||) (map (\word -> word == var) takenNames)) then
--         Parser (\_ -> Left $ "Parse Error: Variable name " ++ var ++ " is already reserved") else return (Var var)



-- znakParser :: Parser Operator2
-- znakParser = defineActionByZnak <$> (satisfy (\char -> elem char ['+', '-', '/', '*', '^']))


-- unaryParser :: Parser Operator1
-- unaryParser = do
--     wordParser "sqrt"

--     return Sqrt


-- unaryOperator :: Integral a => Parser (Expr a)
-- unaryOperator = do
--     operation <- unaryParser
--     separatorParser
--     value <- expressionParser

--     return (Marg operation value)


-- binaryOperator :: Integral a => Parser (Expr a)
-- binaryOperator = do
--     znak <- znakParser
--     separatorParser
--     value1 <- expressionParser
--     separatorParser
--     value2 <- expressionParser

--     return (CE value1 znak value2)


-- expressionParser :: Integral a => Parser (Expr a)
-- expressionParser = binaryOperator <|> unaryOperator <|> parseIndentToExpr <|> parseNumberToExpr


-- fullExpressionParser :: Integral a => Parser (Expr a)
-- fullExpressionParser = do
--     possibleSeparatorParser
--     result <- expressionParser
--     possibleSeparatorParser
--     isFullyApplied

--     return result