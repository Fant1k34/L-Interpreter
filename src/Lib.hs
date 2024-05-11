{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lib where

import Data
import Data.List (lookup)
import Data.Either (Either)

import Control.Monad (Monad(return), foldM)
import Control.Monad.State
import Control.Monad.Trans.State.Lazy (StateT)

import Control.Applicative (Alternative((<|>), empty))
import Data.Foldable1 (foldlM1, Foldable1 (fold1))
import qualified Control.Monad.RWS as StateT
import qualified Control.Monad.RWS as State
import Data.Maybe (mapMaybe)

someFunc :: S Int
someFunc = Seq (Pris (Var "x") (Number 8)) (If (CE (VarAsExpr (Var "x")) Eql (Number 4)) (Write (Str "Okej")) (Write (Str "Not okej")))


getNumberFromE :: (Num a, Show a) => E a -> Either String a
getNumberFromE (Number value) = Right value
getNumberFromE expr = Left $ "Expression " ++ show expr ++ " is not a Number"

getBooleanFromE :: (Num a, Show a) => E a -> Either String Bool
getBooleanFromE (Boolean value) = Right value
getBooleanFromE expr = Left $ "Expression " ++ show expr ++ " is not a Boolean"

getStringFromE :: (Num a, Show a) => E a -> Either String String
getStringFromE (Str value) = Right value
getStringFromE expr = Left $ "Expression " ++ show expr ++ " is not a String"

-- Функция для вычисления выражения. Возвращает либо Строку, либо Выражение (которое должно содержать либо строку, либо число, либо булеан)
-- В состоянии хранится кортеж из списка переменных и их значений, а также из списка функций, доступных для использования
evalExpr :: (Num a, Show a) => E a -> StateT ([(X, a)], [F a]) (Either String) (E a)
evalExpr (VarAsExpr var) = do
    (varList, funList) <- get

    case lookup var varList of
        Just x -> lift $ Right $ Number x
        Nothing -> lift $ Left $ "Variable " ++ show var ++ " does not exist in context"

evalExpr (Number value) = lift $ return $ Number value

evalExpr (Boolean value) = lift $ return $ Boolean value

evalExpr (Str value) = lift $ return $ Str value

evalExpr (CE expr1 op expr2) = do
    -- Получаем состояние
    currentState <- get

    -- Вычисляем левое и правое выражения
    exprEvaluated1 <- evalExpr expr1
    exprEvaluated2 <- evalExpr expr2

    -- Определяем возможные комбинации для оператора op
    -- Например, для операции == мы должны рассмотреть два варианта:
    -- - сравнение двух чисел
    -- - сравнение двух Bool
    -- ...
    -- В результате получаем массив из кортежей, где содержится:
    -- 1. Функция для перевода первого Expression в рассматриваемый тип (функции могут быть разные, например с оператором сравнения можно сначала попробовать перевести всё в тип Number, а потом в Boolean)
    -- 2. Функция для перевода второго Expression в рассматриваемый тип
    -- 3. Функция для кастинга результата операции
    -- 4. Первый Expression
    -- 5. Второй Expression
    let possibleMappingForOperator = definePossibleMapping op exprEvaluated1 exprEvaluated2

    -- Получаем всевозможные результаты вычислений...
    let values = map (\(mappingFirst, mappingSecond, cast, expression1, expression2) ->
            subEval mappingFirst mappingSecond cast expression1 expression2
            ) possibleMappingForOperator

    -- Избавляемся от StateT эффекта за счёт того, что определяем состояние
    let a = map (\el -> evalStateT el currentState) values

    -- Получаем финальный результат: Получилось ли осуществить хоть какое-нибудь преобразование...
    resultFinal <- lift $ foldl (\prev curr -> case prev of
        Right value -> Right value
        Left _ -> curr
        ) (Left "") a

    lift $ return resultFinal

    where
        definePossibleMapping op exprEvaluated1 exprEvaluated2
            | op == Min || op == Del || op == Div || Mod == Mod = [(getNumberFromE, getNumberFromE, Number, exprEvaluated1, exprEvaluated2)]
            | otherwise = [(getNumberFromE, getNumberFromE, Number, exprEvaluated1, exprEvaluated2)]



subEval :: (Num a, Show a) => (E a -> Either String a) -> (E a -> Either String a) -> (a -> E a) -> E a -> E a -> StateT ([(X, a)], [F a]) (Either String) (E a)
subEval mappingFirst mappingSecond cast exprEvaluated1 exprEvaluated2 = do
    result1 <- lift $ mappingFirst exprEvaluated1
    result2 <- lift $ mappingSecond exprEvaluated2

    return $ cast $ result1 + result2
