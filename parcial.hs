-- Jose Manuel Beauregard Mendez A01021716
-- 1
-- Fuerza Bruta
-- [x | x <-[2520..100000000000], mod x 1 == 0,mod x 2 == 0,mod x 3 == 0,mod x 4 == 0, mod x 5 == 0,mod x 6 == 0,mod x 7 == 0,mod x 8 == 0, mod x 9 == 0,mod x 10 == 0,mod x 11 == 0,mod x 12 == 0,mod x 13 == 0,mod x 14 == 0,mod x 15 == 0,mod x 16 == 0,mod x 17 == 0,mod x 18 == 0, mod x 19 == 0, mod x 20 == 0]
-- Funcion Resultado rapido:
foldr1 lcm[1..20]
-- 2
prime n = null [ x | x <- [2..n-1], mod n x  == 0]
res = (filter prime [2..])!!10001

-- 3
maximum [(a * b) | a <- [100..999], b <- [100..999], reverse(show (a * b)) == show(a * b)]

-- 4
data Tree x = Empty | Node x (Tree x) (Tree x) deriving (Show)
printTree Empty = []
printTree (Node x left right) = printTree left ++ [x] ++ printTree right
