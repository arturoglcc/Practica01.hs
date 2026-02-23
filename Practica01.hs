-- Práctica 1. Arturo Guerrero López, Edgar

data List a = Void | Cons a ( List a )

potencia :: Double -> Int -> Double
potencia _ 0 = 1.0
potencia base exp =
    if exp < 0 then 1.0 / potencia base (-exp)
    else  base * potencia  base (exp - 1)
