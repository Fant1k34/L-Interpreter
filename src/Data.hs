{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Data where

data Op2 = Plus | Min | Mult | Del | Div | Mod | Eql | NotEql | More | MoreEql | Less | LessEql | And | Or deriving(Eq)

instance Show Op2 where
    show :: Op2 -> String
    show Plus = "+"
    show Min = "-"
    show Mult = "*"
    show Del = "/"
    show Div = "//"
    show Mod = "%"
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


data E a = VarAsExpr X | Number a | Boolean Bool | Str String | CE (E a) Op2 (E a) deriving(Eq)


instance (Num a, Show a) => Show (E a) where
    show :: (Num a, Show a) => E a -> String
    show (VarAsExpr var) = show var
    show (Number a) = show a
    show (Boolean value) = show value
    show (Str value) = "\"" ++ value ++ "\""
    show (CE expr1 op expr2) = show expr1 ++ " " ++ show op ++ " " ++ show expr2


data A a = Arg (E a) deriving(Eq)

instance (Num a, Show a) => Show (A a) where
    show :: (Num a, Show a) => A a -> String
    show (Arg expr) = "Arg: " ++ show expr


data F a = Fun String [X] (S a) deriving(Eq)

instance (Num a, Show a) => Show (F a) where
    show :: (Num a, Show a) => F a -> String
    show (Fun name arguments statement) = "function " ++ name ++ foldl (\prev curr -> prev ++ " " ++ show curr) "" arguments ++ " <- " ++ show statement


data S a = Pris X (E a) | FunExec (F a) [A a] | Write (E a) | Read X | While (E a) (S a) | If (E a) (S a) (S a) | Seq (S a) (S a) | Skip deriving(Eq)

instance (Num a, Show a) => Show (S a) where
    show :: (Num a, Show a) => S a -> String
    show (Pris var expr) = show var ++ " = " ++ show expr
    show (FunExec (Fun name _ _) arguments) = name ++ foldl (\prev curr -> prev ++ " " ++ show curr) "" arguments
    show (Write expr) = "write " ++ show expr
    show (Read var) = "read " ++ show var
    show (While cond expr) = "while (" ++ show cond ++ ") do " ++ show expr
    show (If cond expr1 expr2) = "if (" ++ show cond ++ ") then (" ++ show expr1 ++ ") else (" ++ show expr2 ++ ")"
    show (Seq statement1 statement2) = show statement1 ++ "; \n" ++ show statement2

-- Example of S:
-- Seq (Pris (Var "x") (Number 8)) ( If (CE (VarAsExpr (Var "x")) Eql (Number 4)) (Write (VarAsExpr (Var "Okej"))) (Write (VarAsExpr (Var "Not okej"))) )
-- Seq (Pris (Var "x") (Number 8)) (If (CE (VarAsExpr (Var "x")) Eql (Number 4)) (Write (Str "Okej")) (Write (Str "Not okej")))
-- It means:
-- x = 8;
-- if (x == 4) then (write Okej) else (write Not okej)

data FDs a = NullFunction | AddFunction (F a) | ConcatFunctions (FDs a) (FDs a) deriving(Eq)

instance (Num a, Show a) => Show (FDs a) where
    show NullFunction = ""
    show (AddFunction function) = show function
    show (ConcatFunctions function1 function2) = (show function1) ++ "\n\n" ++ (show function2)


data P a = Program (FDs a) (S a) deriving(Eq)

instance (Num a, Show a) => Show (P a) where
    show (Program fd s) = show fd ++ "\n\n" ++ show s
