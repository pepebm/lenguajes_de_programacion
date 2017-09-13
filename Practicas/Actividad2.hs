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
