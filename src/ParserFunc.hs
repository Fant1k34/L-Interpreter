{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParserFunc where

import Parser (Parser(..))
import ParserCore
    ( satisfy,
      isFullyApplied,
      parseIndet,
      wordParser,
      separatorParser,
      possibleSeparatorParser,
      parserWithSeparator,
      checkReservedNamesParser,
      Separator(Multiline) )

import Control.Applicative (Alternative((<|>)))

import Data ( F(..), X(..) )

import ParserStatement (parserStatement)


parseFunctionDeclarationToFunc :: Parser (F Float)
parseFunctionDeclarationToFunc = do
    var <- parseIndet
    possibleSeparatorParser Multiline
    checkReservedNamesParser var

    argList <- (\el -> map Var el) <$> (parserWithSeparator parseIndet (separatorParser Multiline))
    -- map checkReservedNamesParser argList

    possibleSeparatorParser Multiline
    wordParser "<-"
    possibleSeparatorParser Multiline

    satisfy (=='{')
    possibleSeparatorParser Multiline

    body <- parserFunc var argList
    case body of
        Fun var' argList' innerFunctions innerBody -> (do
                possibleSeparatorParser Multiline
                satisfy (=='}')

                return (Fun var' argList' innerFunctions innerBody)
            )


parserFunc :: String -> [X] -> Parser (F Float)
parserFunc thisName thisArgs = (do
    listOfInnerFunctions <- parserWithSeparator parseFunctionDeclarationToFunc (possibleSeparatorParser Multiline)

    possibleSeparatorParser Multiline

    body <- parserStatement

    return (Fun thisName thisArgs listOfInnerFunctions body)) <|> (do
        possibleSeparatorParser Multiline
        body <- parserStatement

        return (Fun thisName thisArgs [] body)
        )


parserProgram :: String -> [X] -> Parser (F Float)
parserProgram thisName thisArgs = do
    possibleSeparatorParser Multiline
    code <- parserFunc thisName thisArgs
    possibleSeparatorParser Multiline

    isFullyApplied

    return code

