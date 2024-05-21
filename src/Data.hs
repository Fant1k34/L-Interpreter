{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Data where

data Op2 = Plus | Min | Mult | Del | Eql | NotEql | More | MoreEql | Less | LessEql | And | Or deriving(Eq)

instance Show Op2 where
    show :: Op2 -> String
    show Plus = "+"
    show Min = "-"
    show Mult = "*"
    show Del = "/"
    show Eql = "=="
    show NotEql = "!="
    show More = ">"
    show MoreEql = ">="
    show Less = "<"
    show LessEql = "<="
    show And = "&&"
    show Or = "||"


data X = Var String deriving(Eq)

instance Show X where
    show :: X -> String
    show (Var variable) = variable


data E a = VarAsExpr X | Number a | Boolean Bool | Str String | CE (E a) Op2 (E a) | FunCall String [E a] deriving(Eq)

instance (Show a) => Show (E a) where
    show :: E a -> String
    show (VarAsExpr var) = show var
    show (Number a) = show a
    show (Boolean value) = show value
    show (Str value) = "\"" ++ value ++ "\""
    show (CE (Boolean False) Or expr) = show expr
    show (CE expr1 op expr2) = show expr1 ++ " " ++ show op ++ " " ++ show expr2
    show (FunCall functionName arguments) = functionName ++ foldl (\prev curr -> prev ++ " " ++ show curr) "" arguments

instance Functor E where
    fmap :: (a -> b) -> E a -> E b
    fmap f (VarAsExpr var) = VarAsExpr var
    fmap f (Number a) = Number (f a)
    fmap f (Boolean value) = Boolean value
    fmap f (Str value) = Str value
    fmap f (CE expr1 op expr2) = CE (f <$> expr1) op (f <$> expr2)


-- Fun "first" [(Var "x"), (Var "y")] [] (If (Boolean True) (VarAsExpr (Var "x")) (Number 7))
-- Функция - основной элемент выполнения. В нем могут быть другие функции и так далее
data F a = Fun String [X] [F a] (S a) deriving(Eq)

instance (Num a, Show a) => Show (F a) where
    show :: F a -> String
    show (Fun name arguments listOfFunctions statement) = "function " ++ name ++ foldl (\prev curr -> prev ++ " " ++ show curr) "" arguments ++ " <- " ++ foldl (\prev curr -> prev ++ "\n\t" ++ show curr) "\n\t" listOfFunctions ++ show statement


data S a = ExprAsS (E a) | Pris X (E a) | Write (E a) | ReadStr | While (E a) (S a) | If (E a) (S a) (S a) | Seq (S a) (S a) | Skip deriving(Eq)

instance (Num a, Show a) => Show (S a) where
    show :: S a -> String
    show (Pris var expr) = show var ++ " = " ++ show expr
    show (Write expr) = "write " ++ show expr
    show ReadStr = "read "
    show (While cond expr) = "while (" ++ show cond ++ ") do " ++ show expr
    show (If cond expr1 expr2) = "if (" ++ show cond ++ ") then (" ++ show expr1 ++ ") else (" ++ show expr2 ++ ")"
    show (ExprAsS expr) = show expr
    show (Seq statement1 statement2) = show statement1 ++ "; \n" ++ show statement2
    show Skip = "..."


instance Functor S where
    fmap :: (a -> b) -> S a -> S b
    fmap f (ExprAsS expr) = ExprAsS $ f <$> expr
    fmap f (Pris var expr) = Pris var (f <$> expr)
    fmap f (Write expr) = Write $ f <$> expr
    fmap f ReadStr = ReadStr
    fmap f (While cond expr) = While (f <$> cond) (f <$> expr)
    fmap f (If cond expr1 expr2) = If (f <$> cond) (f <$> expr1) (f <$> expr2)
    fmap f (Seq expr1 expr2) = Seq (f <$> expr1) (f <$> expr2)
    fmap f Skip = Skip
