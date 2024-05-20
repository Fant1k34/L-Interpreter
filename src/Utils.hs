module Utils where


castCharToInt :: Num a => Char -> a
castCharToInt char
    | char == '0' = 0
    | char == '1' = 1
    | char == '2' = 2
    | char == '3' = 3
    | char == '4' = 4
    | char == '5' = 5
    | char == '6' = 6
    | char == '7' = 7
    | char == '8' = 8
    | char == '9' = 9
    | otherwise = error "Inner Error: Something was broken"


razryad :: Integral a => a -> a
razryad value = if value < 10 then 1 else razryad (div value 10) + 1


concatNumbers :: Integral a => a -> a -> a
concatNumbers x y = x * (10 ^ (razryad y)) + y



