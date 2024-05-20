{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, right identity" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Parser where
  
import Control.Applicative (Alternative((<|>), empty) )

-- Парсер из строки в структуру a
newtype Parser a
  = Parser { getParserFunc :: String -> Either String (String, a)}


instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case getParserFunc p input of
      Left comment -> Left comment
      Right (suff, r) -> Right (suff, f r)


instance Applicative Parser where
  pure :: a -> Parser a
  pure res = Parser $ \str -> Right (str, res)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 = Parser (
    \input -> case getParserFunc p1 input of
        Left comment -> Left comment
        Right (suff, transf) -> case getParserFunc p2 suff of
            Left comment -> Left comment
            Right (suff', parsed_value) -> Right (suff', transf parsed_value)
    )


instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p f = Parser (\input ->
    case getParserFunc p input of
        Left comment -> Left comment
        Right (suff, structure_a) -> getParserFunc (f structure_a) suff
    )


instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ -> Left "Parse Error: Empty parser is applied")

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) l r = Parser (\input ->
     case getParserFunc l input of
        Right (suff, result) -> Right (suff, result)
        Left _ -> getParserFunc r input
        )