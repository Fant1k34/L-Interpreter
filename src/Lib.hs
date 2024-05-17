{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lib where

import Data
import Data.List (lookup)
import Data.Either (Either)

import Control.Monad (Monad(return), foldM)
import Control.Monad.State
import Control.Monad.Trans.State.Lazy (StateT, modify)

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
-- В состоянии хранится список из кортежей из списка переменных и их значений, а также из списка функций, доступных для использования
-- [SliceBlock, SliceBlock, ..., SliceBlock] - Глобальный стек выполнения
-- (FunctionsScope, VarScope) - SliceBlock
-- FunctionScope - список из функций в текущем SliceBlock
-- VarScope - список из переменных (область видимости переменных)
-- Последний SliceBlock -- глобальный. Первый - самый вложенный. При запуске функции добавляется новый SliceBlock в Глобальный стек выполнения

-- evalStateT (evalExpr $ CE (CE (Number 5) Plus (VarAsExpr (Var "x"))) Eql (Number 7)) [([], [(Var "x", Number 3)]), ([], [(Var "x", Number 2)])]
evalExpr :: (Floating a, Show a, Ord a) => E a -> StateT [([F a], [(X, E a)])] (Either String) (E a)
evalExpr (VarAsExpr var) = do
    stackEnv <- get

    let result = foldl (\prevValue (_, varList) -> case prevValue of
            Right value -> Right value
            Left comment ->
                case lookup var varList of
                    Just x -> Right x
                    Nothing -> Left comment
                 ) (Left $ "Variable " ++ show var ++ " does not exist in context") stackEnv

    lift result


evalExpr (Number value) = lift $ return $ Number value

evalExpr (Boolean value) = lift $ return $ Boolean value

evalExpr (Str value) = lift $ return $ Str value

evalExpr (CE expr1 op expr2) = do
    exprEvaluated1 <- evalExpr expr1
    exprEvaluated2 <- evalExpr expr2
    let operation = defineOperation op

    lift $ operation exprEvaluated1 exprEvaluated2


-- evalExpr (FunCall funName sentArgs) = do
    -- let newExecutionBlock = ([Fun name args body], zip args sentArgs)

    -- Control.Monad.Trans.State.Lazy.modify (newExecutionBlock :)

    -- result <- evalStatement body

    -- Control.Monad.Trans.State.Lazy.modify (\stackExec -> case stackExec of
        -- [] -> error "Critical Error. Something went wrong. Empty execution stack is not possible"
        -- (h : tail) -> tail
        -- )

    -- lift $ Right result




handlePlus :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handlePlus (Number value1) (Number value2) = Right $ Number $ value1 + value2
handlePlus (Str value1) (Str value2) = Right $ Str $ value1 ++ value2
handlePlus expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " + " ++ show expr2


handleMinus :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleMinus (Number value1) (Number value2) = Right $ Number $ value1 - value2
handleMinus expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " - " ++ show expr2


handleMult :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleMult (Number value1) (Number value2) = Right $ Number $ value1 * value2
handleMult expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " * " ++ show expr2


handleDel :: (Fractional a, Show a) => E a -> E a -> Either String (E a)
handleDel (Number value1) (Number value2) = Right $ Number $ value1 / value2
handleDel expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " / " ++ show expr2


handleEql :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleEql (Number value1) (Number value2) = Right $ Boolean $ value1 == value2
handleEql (Str value1) (Str value2) = Right $ Boolean $ value1 == value2
handleEql (Boolean value1) (Boolean value2) = Right $ Boolean $ value1 == value2
handleEql expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " % " ++ show expr2


handleNotEql :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleNotEql expr1 expr2 = case handleEql expr1 expr2 of
    Right (Boolean value) -> Right $ Boolean $ not value
    Left comment -> Left comment


handleMore :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleMore (Number value1) (Number value2) = Right $ Boolean $ value1 > value2
handleMore expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " > " ++ show expr2


handleMoreEql :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleMoreEql (Number value1) (Number value2) = Right $ Boolean $ value1 >= value2
handleMoreEql expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " >= " ++ show expr2


handleLess :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleLess (Number value1) (Number value2) = Right $ Boolean $ value1 < value2
handleLess expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " < " ++ show expr2


handleLessEql :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleLessEql (Number value1) (Number value2) = Right $ Boolean $ value1 <= value2
handleLessEql expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " <= " ++ show expr2


handleAnd :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleAnd (Boolean value1) (Boolean value2) = Right $ Boolean $ value1 && value2
handleAnd expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " && " ++ show expr2


handleOr :: (Num a, Ord a, Show a) => E a -> E a -> Either String (E a)
handleOr (Boolean value1) (Boolean value2) = Right $ Boolean $ value1 || value2
handleOr expr1 expr2 = Left $ "No possible operation for: " ++ show expr1 ++ " || " ++ show expr2


defineOperation :: (Floating a, Show a, Ord a) => Op2 -> (E a -> E a -> Either String (E a))
defineOperation op
            | op == Plus = handlePlus
            | op == Min = handleMinus
            | op == Mult = handleMult
            | op == Del = handleDel
            | op == Eql = handleEql
            | op == NotEql = handleNotEql
            | op == More = handleMore
            | op == MoreEql = handleMoreEql
            | op == Less = handleLess
            | op == LessEql = handleLessEql
            | op == And = handleAnd
            | op == Or = handleOr
            | otherwise = \_ _ -> Left "Unsupported operation"


subEval :: (Num a, Show a) => (E a -> Either String a) -> (E a -> Either String a) -> (a -> E a) -> E a -> E a -> (a -> a -> a) -> Either String (E a)
subEval mappingFirst mappingSecond cast exprEvaluated1 exprEvaluated2 op = do
    result1 <- mappingFirst exprEvaluated1
    result2 <- mappingSecond exprEvaluated2

    return $ cast $ op result1 result2


evalStatement :: (Floating a, Show a, Ord a) => S a -> StateT [([F a], [(X, E a)])] (Either String) (E a)
evalStatement statement = undefined
