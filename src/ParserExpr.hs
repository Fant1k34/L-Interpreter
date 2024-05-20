{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module ParserExpr where

import Parser (Parser(..))
import ParserCore

import Utils (razryad, concatNumbers, castCharToInt)

import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
import Control.Applicative (Alternative((<|>), empty))

import Data
import Data.Foldable1 (foldlM1)

-- import Basement.Floating
import GHC.Integer (Integer)
import GHC.Real (Integral)

takenNames :: [String]
takenNames = ["True", "False", "function", "Write", "Read", "While", "If", "Skip"]


defineActionByZnak :: String -> Op2
defineActionByZnak znak
    | znak == "-" = Min
    | znak == "+" = Plus
    | znak == "*" = Mult
    | znak == "/" = Del
    | znak == "==" = Eql
    | znak == "!=" = NotEql
    | znak == ">" = More
    | znak == ">=" = MoreEql
    | znak == "<" = Less
    | znak == "<=" = LessEql
    | znak == "&&" = And
    | znak == "||" = Or
    | otherwise = error "Inner Error: Something was broken during parsing. defineActionByZnak got not action"


parseNumberToExpr :: Parser (E Float)
parseNumberToExpr = (do
    number <- parseFloat

    return (Number number)
    ) <|> (do
    number <- parseNumber

    return (Number $ fromInteger number)
    )


parseIndentToExpr :: Parser (E a)
parseIndentToExpr = do
    var <- parseIndet

    if var `elem` takenNames then
        Parser (\_ -> Left $ "Parse Error: Variable name " ++ var ++ " is already reserved") else return (VarAsExpr (Var var))



znakParser :: Parser Op2
znakParser = defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["+", "-", "/", "*", "==", "!=", ">=", "<=", ">", "<", "&&", "||"]


parseStrToExpr :: Parser (E a)
parseStrToExpr = do
    satisfy (=='\"')
    content <- ParserCore.any (satisfy (/= '\"'))
    satisfy (=='\"')

    return (Str content)


parseBooleanToExpr :: Parser (E a)
parseBooleanToExpr = (\parsed -> if parsed == "True" then Boolean True else Boolean False) <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["True", "False"]


sequenceParser :: Parser (E a) -> Parser Op2 -> Parser (E a)
sequenceParser p opP = (do
    value <- p
    operator <- opP

    leftover <- sequenceParser p opP

    return (CE value operator leftover)
    ) <|> (do
    value <- p

    return value
    )


expressionParserL4 :: Parser (E Float)
expressionParserL4 = do
    sequenceParser (parseNumberToExpr <|> parseStrToExpr <|> parseBooleanToExpr <|> parseIndentToExpr) (do
        possibleSeparatorParser
        appliedOp <- defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["*", "/"]
        possibleSeparatorParser

        return appliedOp
        )

expressionParserL3 :: Parser (E Float)
expressionParserL3 = do
    sequenceParser expressionParserL4 (do
        possibleSeparatorParser
        appliedOp <- defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["+", "-"]
        possibleSeparatorParser

        return appliedOp
        )


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