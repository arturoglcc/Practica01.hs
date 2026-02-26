-- data List a = Void | Cons a ( List a ) deriving (Show, Eq)␍


-- Funcion que regresa la cabeza de la lista.␍
myHead :: List a -> Maybe a␍
-- Simula la lista vacia con nuestra definicion␍
myHead Void = Nothing␍
-- Como estamos trabajando on Maybe tenemos que regresar Just␍
myHead (Cons x _) = Just x␍

--Funcion que regresa tal vez el ultimo elemento de la lista.␍
myLast :: List a -> Maybe a␍
myLast Void = Nothing␍
myLast (Cons x Void) = Just x␍
myLast (Cons _ xs) = myLast xs␍


--Funcion que nos dice si un elemento esta en una lista.␍
isElem :: (Eq a) => List a -> a -> Bool␍
isElem Void _ = False␍
isElem (Cons x xs) a | x == a = True␍
                     | otherwise = isElem xs a␍
                   ␍
--Función que pasa una de nuestras listas a las listas de haskell.␍
toHaskell :: List a -> [a ]␍
toHaskell Void = []␍
toHaskell (Cons x xs) = x : toHaskell xs ␍

-- Data que representa la representacion de formulas de la logica proposicional␍
data Formula = Var String | Neg Formula  | And Formula Formula | Or Formula Formula | Imp Formula Formula deriving (Show, Eq)
