{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module ParserExpr (parserExpr) where

import Parser (Parser(..))
import ParserCore

import Control.Applicative (Alternative((<|>), empty))

import Data

-- import Basement.Floating

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
    ) <|> (do
    satisfy (=='-')
    possibleSeparatorParser
    number <- parseFloat

    return (Number (number * (-1)))
    ) <|> (do
    satisfy (=='-')
    possibleSeparatorParser
    number <- parseNumber

    return (Number $ fromInteger (number * (-1)))
    )


parseIndentToExpr :: Parser (E a)
parseIndentToExpr = do
    var <- parseIndet
    checkReservedNamesParser var

    return (VarAsExpr (Var var))


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


parseFunctionCallToExpr :: Parser (E Float)
parseFunctionCallToExpr = do
    funName <- parseIndet
    separatorParser
    args <- parserWithSeparator parserExpr separatorParser

    return (FunCall funName args)



sequenceParser :: Parser (E a) -> Parser Op2 -> Parser (E a)
sequenceParser p opP = sequenceParser' p opP (Boolean False) Or


sequenceParser' :: Parser (E a) -> Parser Op2 -> E a -> Op2 -> Parser (E a)
sequenceParser' p opP leftValue currentOp = (do
    value <- p
    operator <- opP

    let fullExpr = CE leftValue currentOp value

    sequenceParser' p opP fullExpr operator
    ) <|> (do
    value <- p

    return $ CE leftValue currentOp value
    )


expressionParserL4 :: Parser (E Float)
expressionParserL4 = do
    sequenceParser (parseNumberToExpr <|> parseStrToExpr <|> parseBooleanToExpr <|> parseFunctionCallToExpr <|> parseIndentToExpr) (do
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

expressionParserL2 :: Parser (E Float)
expressionParserL2 = do
    sequenceParser expressionParserL3 (do
        possibleSeparatorParser
        appliedOp <- defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["!=", "==", ">=", "<=", ">", "<"]
        possibleSeparatorParser

        return appliedOp
        )


expressionParserL1 :: Parser (E Float)
expressionParserL1 = do
    sequenceParser expressionParserL2 (do
        possibleSeparatorParser
        appliedOp <- defineActionByZnak <$> wordParser "&&"
        possibleSeparatorParser

        return appliedOp
        )


expressionParserL0 :: Parser (E Float)
expressionParserL0 = do
    sequenceParser expressionParserL1 (do
        possibleSeparatorParser
        appliedOp <- defineActionByZnak <$> wordParser "||"
        possibleSeparatorParser

        return appliedOp
        )


parserExpr :: Parser (E Float)
parserExpr = do
    expr <- expressionParserL0

    return expr
