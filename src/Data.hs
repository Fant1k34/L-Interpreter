module Data where

data Op2 = Plus | Min | Mult | Del | Div | Mod | Eql | NotEql | More | MoreEql | Less | LessEql | And | Or deriving(Eq)

instance Show Op2 where
    -- show :: Op2 -> String
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
    -- show :: X -> String
    show (Var variable) = variable


data E a = AsExpr X | Val a | CE (E a) Op2 (E a) deriving(Eq)

instance (Num a, Show a) => Show (E a) where
    -- show ::  E a -> String
    show (AsExpr var) = show var
    show (Val a) = show a
    show (CE expr1 op expr2) = show expr1 ++ " " ++ show op ++ " " ++ show expr2


data A a = Arg (E a) deriving(Eq)

instance (Num a, Show a) => Show (A a) where
    -- show :: (Num a, Show a) => A a -> String
    show (Arg expr) = "Arg: " ++ show expr


data F a = Fun String [X] (S a) deriving(Eq)

instance (Num a, Show a) => Show (F a) where
    -- show :: (Num a, Show a) => F a -> String
    show (Fun name arguments statement) = "function " ++ name ++ (foldl (\prev curr -> prev ++ " " ++ show curr) "" arguments) ++ " <- " ++ show statement


data S a = Pris X (E a) | FunExec (F a) [A a] | Write (E a) | Read X | While (E a) (S a) | If (E a) (S a) (S a) | Seq (S a) (S a) | Skip deriving(Eq, Show)
