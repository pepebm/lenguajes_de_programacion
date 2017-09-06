-- Jose Manuel Beauregard Mendez A01021716

data Point = Point Float Float deriving (Show)

data Shape = Circle Float | Rectangle Float Float Float Float | RectP Point Point | Triangule Float Float deriving (Show)

area (Circle r) = pi * r * r

area (RectP (Point x1 y1) (Point x2 y2)) = (x2-x1) * (y2-y1)

area (Rectangle x1 y1 x2 y2) = (x2-x1) * (y2-y1)

area (Triangule b h) = (b*h)/2

data Date = Date {day::Int, month::String, year::Int} deriving (Show, Read)
-- let x = read "Date{day=2,month=\"September\",year=2017}"::Date
-- getDay (Date d m y) = d
-- getMonth (Date d m y) = m
-- getYear(Date d m y) = y

data Pair key value = Pair key value deriving (Show)

data Lista a = Vacia | Concat a (Lista a) deriving (Show)


data Tree x = Empty | Node x (Tree x) (Tree x) deriving (Show)

insertt x Empty = Node x Empty Empty

insertt x (Node z left right)
          | x==z      =Node z left right
          | x<z       =Node z (insertt x left) right
          | otherwise =Node z left (insertt x right)


treeList l tree
          | l==[] =tree
          | otherwise =treeList (tail l) (insertt (head l) tree)

createTree l = treeList l Empty
