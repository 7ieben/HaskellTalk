module Vortrag where

-- für Listenfunktionen wie sort
import Data.List

-- Folie 3
sortLines t = unlines (sort (lines t))

sortLines' t = (unlines . sort . lines) t

sortLines'' = unlines . sort . lines

-- Folie 4

reverseLines = unlines . reverse . lines

first2Lines = unlines . take 2 . lines

byLines f = unlines . f . lines

-- Folie 5 und 6

add :: Num a => a -> a -> a
add x y = x + y

-- add 3 4
-- (add 3) 4

-- Folie 7

add3 y = 3 + y
add3' = (+) 3

add3onLists l = (map add3) l
add3onLists' = map add3

-- Folie 8

data MyList α = NIL
              | Cons α (MyList α)
                deriving Show

list1 = NIL
list2 = Cons "a" list1
list3 = Cons "b" list2
list4 = Cons "a" list4

-- Folie 9/10

car :: MyList a -> a
car (Cons x xs) = x
car NIL = error "Damn it!"

cdr :: MyList a -> MyList a
cdr (Cons x xs) = xs
cdr NIL = NIL

-- Folie 11/12

--data Maybe α = Just α
--	       | Nothing
-- Maybe ist bereits vordefiniert, deshalb hier als Kommentar

car' :: MyList a -> Maybe a
car' (Cons x xs) = Just x
car' NIL = Nothing

cdr' :: MyList a -> Maybe (MyList a)
cdr' (Cons x xs) = Just xs
cdr' NIL = Nothing

-- Folie 12

-- cadr x = (car . cdr) x
-- Führt zu Typfehler

-- Folie 13

-- (>>=) :: m a -> (a -> m b) -> m b

cadr :: MyList a -> Maybe a
cadr x = cdr' x >>= car' 
