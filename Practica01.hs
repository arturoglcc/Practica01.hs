-- Práctica 1. Arturo Guerrero López, Edgar Jesús Morales Martínez

data List a = Void | Cons a ( List a ) deriving (Show, Eq)

--ejercicio 1
potencia :: Double -> Int -> Double
potencia _ 0 = 1.0
potencia base exp =
    if exp < 0 then 1.0 / potencia base (-exp)
    else  base * potencia  base (exp - 1)

-- Funcion que regresa la cabeza de la lista.␍
myHead :: List a -> Maybe a
-- Simula la lista vacia con nuestra definicion␍
myHead Void = Nothing
-- Como estamos trabajando on Maybe tenemos que regresar Just␍
myHead (Cons x _) = Just x

--ejercicio 3
myTail :: List a -> Maybe ( List a )
myTail Void = Nothing
myTail (Cons _ xs) = Just xs

--Funcion que regresa tal vez el ultimo elemento de la lista.␍
myLast :: List a -> Maybe a
myLast Void = Nothing
myLast (Cons x Void) = Just x
myLast (Cons _ xs) = myLast xs

--ejercicio 5
myReverse :: List a -> List a
myReverse Void = Void
myReverse (Cons x xs) = append (myReverse xs) (Cons x Void)

append :: List a -> List a -> List a
append Void ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

--Funcion que nos dice si un elemento esta en una lista.
isElem :: (Eq a) => List a -> a -> Bool
isElem Void _ = False
isElem (Cons x xs) a | x == a = True
                     | otherwise = isElem xs a

--ejercicio 7
myLen :: List a -> Int
myLen Void = 0
myLen (Cons _ xs) = 1 + myLen(xs)

--Función que pasa una de nuestras listas a las listas de haskell.
toHaskell :: List a -> [a ]
toHaskell Void = []
toHaskell (Cons x xs) = x : toHaskell xs

--ejercicio 9
fromHaskell :: [ a ] -> List a
fromHaskell [] = Void
fromHaskell (x:xs) = Cons x (fromHaskell xs)

-- Data que representa la representacion de formulas de la logica proposicional␍
data Formula = Var String | Neg Formula  | And Formula Formula | Or Formula Formula | Imp Formula Formula deriving (Show, Eq)
