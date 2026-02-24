-- Práctica 1. Arturo Guerrero López, Edgar

data List a = Void | Cons a ( List a ) deriving (Show, Eq)

potencia :: Double -> Int -> Double
potencia _ 0 = 1.0
potencia base exp =
    if exp < 0 then 1.0 / potencia base (-exp)
    else  base * potencia  base (exp - 1)

myTail :: List a -> Maybe ( List a )
myTail Void = Nothing
myTail (Cons _ xs) = Just xs

myLen :: List a -> Int
myLen Void = 0
myLen (Cons _ xs) = 1 + myLen(xs)

myReverse :: List a -> List a
myReverse Void = Void
myReverse (Cons x xs) = append (myReverse xs) (Cons x Void)

append :: List a -> List a -> List a
append Void ys = ys
append (Cons x xs) ys = Cons x (append xs ys)
