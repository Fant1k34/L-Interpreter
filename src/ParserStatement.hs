{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module ParserStatement (parserStatement) where

import Parser (Parser(..))
import ParserCore
    ( satisfy,
      some,
      parseIndet,
      wordParser,
      separatorParser,
      possibleSeparatorParser,
      parserWithSeparator,
      checkReservedNamesParser,
      Separator(Multiline, Inline) )

import Control.Applicative (Alternative((<|>), empty))

import Data ( S(..), X(Var) )

import ParserExpr (parserExpr)


parsePrisToStatement :: Parser (S Float)
parsePrisToStatement = do
    varName <- parseIndet
    possibleSeparatorParser Inline
    wordParser ":="
    possibleSeparatorParser Inline
    varValue <- parserExpr

    checkReservedNamesParser varName

    return (Pris (Var varName) varValue)


parseWriteToStatement :: Parser (S Float)
parseWriteToStatement = do
    wordParser "write"
    separatorParser Inline
    expr <- parserExpr

    return (Write expr)


parseReadStrToStatement :: Parser (S Float)
parseReadStrToStatement = do
    wordParser "read"

    return ReadStr

parseWhileToStatement :: Parser (S Float)
parseWhileToStatement = do
    wordParser "while"
    separatorParser Multiline
    cond <- parserExpr
    separatorParser Multiline

    wordParser "do"
    separatorParser Multiline
    satisfy (=='{')
    possibleSeparatorParser Multiline
    body <- parserStatement
    possibleSeparatorParser Multiline
    satisfy (=='}')

    return (While cond body)


parseIfToStatement :: Parser (S Float)
parseIfToStatement = do
    wordParser "if"
    separatorParser Multiline
    cond <- parserExpr
    separatorParser Multiline

    wordParser "then"
    separatorParser Multiline
    satisfy (=='{')
    possibleSeparatorParser Multiline
    bodyThen <- parserStatement
    possibleSeparatorParser Multiline
    satisfy (=='}')
    separatorParser Multiline

    wordParser "else"
    separatorParser Multiline
    satisfy (=='{')
    possibleSeparatorParser Multiline
    bodyElse <- parserStatement
    possibleSeparatorParser Multiline
    satisfy (=='}')

    return (If cond bodyThen bodyElse)


parseSkipToStatement :: Parser (S Float)
parseSkipToStatement = do
    wordParser "skip"

    return Skip


parseExprToStatement :: Parser (S Float)
parseExprToStatement = do
    expr <- parserExpr

    return $ ExprAsS expr


parserStatement :: Parser (S Float)
parserStatement = do
    statements <- parserWithSeparator (parsePrisToStatement <|> parseWriteToStatement <|> parseReadStrToStatement <|> parseWhileToStatement <|> parseIfToStatement <|> parseSkipToStatement <|> parseExprToStatement)
        (do
            ParserCore.some (satisfy (=='\n') <|> satisfy (=='\t') <|> satisfy (==' '))
        )

    case statements of
        [] -> empty
        [element] -> return element
        list -> return $ foldl (\prev curr -> Seq prev curr) (Seq Skip Skip) list
