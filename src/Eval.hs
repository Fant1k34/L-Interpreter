{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Eval (evalExpr, evalStatement, evalFunc) where

import Data (F(..), E(..), S(..), X(..), Op2(..))
import EvalHandlers (defineOperation)

import Data.List (lookup)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Control.Monad.State (MonadState(get), MonadTrans(lift), StateT)
import Control.Monad.Trans.State.Lazy (modify)
import Control.Monad.Trans.IO

import qualified Control.Monad.RWS as StateT


-- Функция для вычисления выражения.
-- Возвращает либо Строку, либо Выражение (которое должно содержать либо строку, либо число, либо булеан)
-- В состоянии хранится список из кортежей из списка переменных и их значений,
--                       а также из списка функций, доступных для использования
-- [SliceBlock_N, SliceBlock_(N-1), ..., SliceBlock_1] - Глобальный стек выполнения
--                                  (FunctionsScope_for_N, VarScope_for_N) - SliceBlock_N
--                                   * FunctionsScope - список из функций в текущем SliceBlock
--                                   * VarScope - список из переменных (область видимости переменных)
-- Последний SliceBlock -- глобальный, а Первый - самый вложенный
-- При запуске функции добавляется новый SliceBlock в Глобальный стек выполнения
evalExpr :: (Floating a, Show a, Ord a) => E a -> StateT [([F a], [(X, E a)])] (IOT (Either String)) (E a)
evalExpr (VarAsExpr var) = do
    stackEnv <- get

    let result = foldl (\prevValue (_, varList) -> case prevValue of
            Right value -> Right value
            Left comment ->
                case lookup var varList of
                    Just x -> Right x
                    Nothing -> Left comment
                 ) (Left $ "Variable " ++ show var ++ " does not exist in context") stackEnv

    StateT.lift $ lift result

evalExpr (Number value) = StateT.lift $ lift $ Right $ Number value

evalExpr (Boolean value) = StateT.lift $ lift $ Right $ Boolean value

evalExpr (Str value) = StateT.lift $ lift $ Right $ Str value


-- Делаем фишку с lazy computations внутри условий (как в JavaScript... Иногда она спасает...)
-- Если первый аргумент False и оператор && - мы скипаем вычисление всего условия
-- Если первый аргумент False и оператор || - мы вычисляем второе выражение и возвращаем его тип
--      Что теперь мы можем написать (пример из ReactJS):
--               (mode == "Manager") && <div>Это окно видит только менеджер</div>
evalExpr (CE (Boolean False) op expr2) = case op of
    And -> StateT.lift $ lift $ Right (Boolean False)
    Or -> (do
        evalExpr expr2
        )
    _ -> StateT.lift $ lift $ Left $ "Wrong condition for expression: False " ++ (show op) ++ (show expr2)


-- Делаем фишку с lazy computations внутри условий (как в JavaScript... Иногда она спасает...)
-- Если первый аргумент True и оператор || - мы скипаем вычисление всего условия
-- Если первый аргумент True и оператор && - мы вычисляем второе выражение и возвращаем его тип
evalExpr (CE (Boolean True) op expr2) = case op of
    Or -> StateT.lift $ lift $ Right (Boolean True)
    And -> (do
        evalExpr expr2
        )
    _ -> StateT.lift $ lift $ Left $ "Wrong condition for expression: True " ++ (show op) ++ (show expr2)


evalExpr (CE expr1 op expr2) = do
    exprEvaluated1 <- evalExpr expr1
    exprEvaluated2 <- evalExpr expr2
    let operation = defineOperation op

    StateT.lift $ lift $ operation exprEvaluated1 exprEvaluated2

-- TODO: Подумать, как запретить создавать функции с одинаковыми именами
evalExpr (FunCall funCallName sentArgs) = do
    stackEnv <- get
    evaledArguments <- mapM evalExpr sentArgs

    let function = foldl (\prevValue (funList, _) -> case prevValue of
            Right value -> Right value
            Left comment ->
                case filter (\ (Fun name _ _ _) -> name == funCallName) funList of
                    (foundFunction : _) -> Right foundFunction
                    [] -> Left comment
                 ) (Left $ "Function " ++ show funCallName ++ " does not exist in context") stackEnv

    case function of
        Left comment -> StateT.lift $ lift $ Left comment
        Right fun -> evalFunc fun evaledArguments


evalStatement :: (Floating a, Show a, Ord a) => S a -> StateT [([F a], [(X, E a)])] (IOT (Either String)) (E a)
evalStatement (ExprAsS expr) = evalExpr expr

evalStatement (If cond s1 s2) = do
    result <- evalExpr cond

    case result of
        Boolean value -> if value then evalStatement s1 else evalStatement s2
        notBoolean -> StateT.lift $ lift $ Left $ "If condition may be applied only to boolean expressions, but got " ++ show notBoolean

evalStatement (Seq s1 s2) = do
    evalStatement s1
    evalStatement s2

evalStatement (Write value) = do
    valueToWrite <- evalExpr value
    filename <- evalExpr (VarAsExpr (Var "outputFile"))

    StateT.lift $ fromIO $ (\() -> Right $ Str $ show value) <$> print (show valueToWrite)

    case filename of
        (Str name) -> StateT.lift $ fromIO $ (\() -> Right $ Str $ show value) <$> writeFile name (show valueToWrite)
        _ -> StateT.lift $ lift $ Left "Variable outputFile is overriden wrongly, no possibility to write to file"

evalStatement ReadStr = do
    input <- StateT.lift $ fromIO $ Right . Str <$> getLine

    StateT.lift $ lift $ Right input

evalStatement (Pris var expr) = do
    stackEnv <- get
    evaluatedExpr <- evalExpr expr

    let (functionList, varList) = head stackEnv

    let modifedVarList = case lookup var varList of
            Nothing -> (var, evaluatedExpr) : varList
            Just _ -> foldl (\prev (key, value) -> 
                if key == var then (key, evaluatedExpr) : prev else (key, value) : prev
                    ) [] varList

    let modifedStack = (functionList, modifedVarList)

    Control.Monad.Trans.State.Lazy.modify (\stackExec -> case stackExec of
        [] -> error "Critical Error. Something went wrong. Execution stack cannot be empty"
        (_ : tailOfStackExec) -> tailOfStackExec
        )
    
    Control.Monad.Trans.State.Lazy.modify (modifedStack :)

    StateT.lift $ lift $ Right expr

evalStatement (While cond body) = do
    result <- evalExpr cond

    case result of
        Boolean value -> if value then (do
            evalStatement body
            evalStatement (While cond body)
            ) else StateT.lift $ lift $ Right $ Boolean False
        notBoolean -> StateT.lift $ lift $ Left $ "If condition may be applied only to boolean expressions, but got " ++ show notBoolean


evalFunc :: (Floating a, Show a, Ord a) => F a -> [E a] -> StateT [([F a], [(X, E a)])] (IOT (Either String)) (E a)
evalFunc (Fun name args innerFunctions body) sentArgs = do
    let newExecutionBlock = (innerFunctions, zip args sentArgs)

    Control.Monad.Trans.State.Lazy.modify (newExecutionBlock :)

    result <- evalStatement body

    Control.Monad.Trans.State.Lazy.modify (\stackExec -> case stackExec of
        [] -> error "Critical Error. Something went wrong. Execution stack cannot be empty"
        (_ : tailOfStackExec) -> tailOfStackExec
        )

    StateT.lift $ lift $ Right result
