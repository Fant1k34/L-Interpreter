{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ParserCore (satisfy, some, ParserCore.any, isFullyApplied, parseInt, parseFloat, parseIndet, wordParser, separatorParser, possibleSeparatorParser, parserWithSeparator, succesParser, checkReservedNamesParser, Separator(..)) where


import Parser (Parser(..))

import Utils ( castCharToInt, concatNumbers )

import Data.Char (isAlpha, isAlphaNum, isNumber)
import Control.Applicative (Alternative((<|>), empty))


-- Функция принимает условие для первого символа строки внутри парсера
-- Возвращает парсер, который при удачном применении (если
-- условие на первом элементе выполняется) возвращает Char
satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = Parser (\input ->
    case input of
        (h : t) -> if cond h then Right (t, h) else Left ("Parse Error: Predicate is not followed on " ++ show h ++ " in input " ++ input)
        _ -> Left "Parse Error: Empty input"
        )


-- Пустой парсер, который заканчивается всегда успехом
succesParser :: Parser String
succesParser = Parser (\input -> Right ("", input))


-- Функция принимает Парсер
-- Возвращает парсер, который применяет 1 или более раз парсер и возвращает список из результатов
some :: Parser a -> Parser [a]
some p1 = (p1 >>= (\successResultP1 ->
    Parser ( \input -> case getParserFunc (some p1) input of
        Left comment -> Right (input, [successResultP1])
        Right (suff', value) -> Right (suff', [successResultP1] ++ value) ) ) ) <|> empty


-- Функция принимает Парсер
-- Возвращает парсер, который применяет 0 или более раз парсер и возвращает список из результатов парсинга
-- Если парсер применён 0 раз, то возвращает пустой список
any :: Parser a -> Parser [a]
any p1 = some p1 <|> return []


-- Функция, которая возвращает парсер, который говорит о том, осталось ли часть строки неотпарсенной
isFullyApplied :: Parser ()
isFullyApplied = Parser (\input -> if (length input == 0) then Right (input, ()) else Left ("Parse Error: String is not fully parsed -- " ++ input))


-- Функция парсинга целых чисел
parseInt :: Parser Integer
parseInt = do
    numberList <- (castCharToInt <$>) <$> (some (satisfy isNumber))
    let result = Prelude.foldl1 concatNumbers numberList

    return result


-- Функция парсинга дробных чисел
parseFloat :: (Floating a) => Parser a
parseFloat = do
    numberList1 <- ((fromInteger . castCharToInt) <$>) <$> some (satisfy isNumber)
    satisfy (== '.')
    numberList2 <- ((fromInteger . castCharToInt) <$>) <$> some (satisfy isNumber)

    let result1 = Prelude.foldl1 concatNumbers numberList1
    let result2 = Prelude.foldl1 concatNumbers numberList2

    let modifiedResult2 = fromInteger result2 / fromInteger (10 ^ length numberList2)

    return $ fromInteger result1 + modifiedResult2


-- Функция парсинга имен переменных
parseIndet :: Parser [Char]
parseIndet = do
    letter <- satisfy isAlpha
    other <- ParserCore.any (satisfy isAlphaNum)
    return (letter : other)


-- Функция принимает строку и возвращает парсер, который парсит строку
wordParser :: String -> Parser String
wordParser word = if (length word == 0) then return "" else foldl1 (\p1 p2 -> p1 >>= (\successP1 -> Parser (\input ->
    case (getParserFunc p2 input) of
        Left comment -> Left comment
        Right (suff, value) -> Right (suff, successP1 ++ value)))) (map (\char -> ((: []) <$> (satisfy (==char)))) word)

-- Inline: " " or "\t"
-- Multiline: Inline + "\n"
data Separator = Inline | Multiline

-- Парсер разделителей
separatorParser :: Separator -> Parser String
separatorParser sep = case sep of 
    Inline -> some (satisfy (\el -> el == ' ' || el == '\t'))
    Multiline -> some (satisfy (\el -> el == ' ' || el == '\t' || el == '\n'))


-- Парсер возможных разделителей
possibleSeparatorParser :: Separator -> Parser String
possibleSeparatorParser sep = separatorParser sep <|> return ""


parserWithSeparator :: Parser a -> Parser b -> Parser [a]
parserWithSeparator p sepP = (do
    value <- p
    sepP
    results <- parserWithSeparator p sepP

    return (value : results)
    ) <|> (do
    value <- p

    return [value]
    )


checkReservedNamesParser :: String -> Parser String
checkReservedNamesParser word = Parser $ \input -> 
    let takenNames = ["True", "False", "function", "write", "read", "while", "if", "skip", "do", "then", "else"]
        in
        if word `elem` takenNames then 
            Left $ "Parse Error: Variable name " ++ word ++ " is already reserved" 
            else Right (input, "")
