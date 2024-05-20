module EvalHandlers (defineOperation) where

import Data (E(..), Op2(..))

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