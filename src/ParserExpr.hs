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
    ( satisfy,
      any,
      parseFloat,
      parseIndet,
      wordParser,
      possibleSeparatorParser,
      parserWithSeparator,
      checkReservedNamesParser,
      parseInt,
      Separator(..) )

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
    number <- parseInt

    return (Number $ fromInteger number)
    ) <|> (do
    satisfy (=='-')
    possibleSeparatorParser Inline
    number <- parseFloat

    return (Number (number * (-1)))
    ) <|> (do
    satisfy (=='-')
    possibleSeparatorParser Inline
    number <- parseInt

    return (Number $ fromInteger (number * (-1)))
    )


parseIndentToExpr :: Parser (E a)
parseIndentToExpr = do
    var <- parseIndet
    checkReservedNamesParser var

    return (VarAsExpr (Var var))


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
    possibleSeparatorParser Inline

    satisfy (=='(')
    possibleSeparatorParser Inline
    args <- parserWithSeparator parserExpr (do
        possibleSeparatorParser Inline
        wordParser ","
        possibleSeparatorParser Inline
        )
    possibleSeparatorParser Inline
    satisfy (==')')

    return (FunCall funName args)


optimize :: E a -> E a
optimize (CE expr1 op expr2) = do
    let optExpr1 = case optimize expr1 of
            CE expr1' op' expr2' -> CE (optimize expr1') op' (optimize expr2')
            result -> result

    let optExpr2 = case optimize expr2 of
            CE expr1' op' expr2' -> CE (optimize expr1') op' (optimize expr2')
            result -> result
    
    case optExpr1 of
        Boolean False | op == Or -> optExpr2
        Boolean True | op == And -> optExpr2
        _ -> case optExpr2 of
                Boolean False | op == Or -> optExpr1
                Boolean True | op == And -> optExpr1
                _ -> CE optExpr1 op optExpr2
    
    
optimize other = other



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


expressionParserL5 :: Parser (E Float)
expressionParserL5 = do
    parseNumberToExpr <|> parseStrToExpr <|> parseBooleanToExpr <|> parseFunctionCallToExpr <|> parseIndentToExpr <|> (do
        satisfy (=='(')
        possibleSeparatorParser Inline
        expr <- parserExpr
        possibleSeparatorParser Inline
        satisfy (==')')

        return expr
        ) <|> (do
        satisfy (=='-')
        possibleSeparatorParser Inline
        satisfy (=='(')
        possibleSeparatorParser Inline
        expr <- parserExpr
        possibleSeparatorParser Inline
        satisfy (==')')

        return $ CE (Number $ -1) Mult expr 
        )



expressionParserL4 :: Parser (E Float)
expressionParserL4 = do
    sequenceParser expressionParserL5 (do
        possibleSeparatorParser Inline
        appliedOp <- defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["*", "/"]
        possibleSeparatorParser Inline

        return appliedOp
        )



expressionParserL3 :: Parser (E Float)
expressionParserL3 = do
    sequenceParser expressionParserL4 (do
        possibleSeparatorParser Inline
        appliedOp <- defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["+", "-"]
        possibleSeparatorParser Inline

        return appliedOp
        )


expressionParserL2 :: Parser (E Float)
expressionParserL2 = do
    sequenceParser expressionParserL3 (do
        possibleSeparatorParser Inline
        appliedOp <- defineActionByZnak <$> foldl (\prev curr -> prev <|> wordParser curr) empty ["!=", "==", ">=", "<=", ">", "<"]
        possibleSeparatorParser Inline

        return appliedOp
        )


expressionParserL1 :: Parser (E Float)
expressionParserL1 = do
    sequenceParser expressionParserL2 (do
        possibleSeparatorParser Inline
        appliedOp <- defineActionByZnak <$> wordParser "&&"
        possibleSeparatorParser Inline

        return appliedOp
        )


expressionParserL0 :: Parser (E Float)
expressionParserL0 = do
    sequenceParser expressionParserL1 (do
        possibleSeparatorParser Inline
        appliedOp <- defineActionByZnak <$> wordParser "||"
        possibleSeparatorParser Inline

        return appliedOp
        )


parserExpr :: Parser (E Float)
parserExpr = do
    expr <- expressionParserL0

    return $ optimize expr
