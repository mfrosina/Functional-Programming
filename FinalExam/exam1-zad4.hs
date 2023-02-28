main :: IO()
main = do 
    --print (areCousins t1 2 3)
    print (areCousins t1 2 3)
    print (areCousins t2 8 4)

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 :: BTree Int
t1 = Node 1 (Node 2 Nil Nil)
    (Node 3 Nil Nil)
t2 :: BTree Int
t2 = Node 5 (Node 3 (Node 4 Nil Nil)
 (Node 7 Nil Nil))
 (Node 2 (Node 9 Nil Nil)
 (Node 8 Nil Nil))

getDepth :: (Eq a,Ord a) => BTree a -> a -> Int
getDepth Nil _ = -1
getDepth t@(Node v left right) el = helper t 0 el 
 where 
     helper Nil _ _ = -1
     helper (Node v left right) d el
      |v == el = d 
      |otherwise = max (helper left (d+1) el) (helper right (d+1) el)

findParent :: (Num a,Eq a,Ord a) => BTree a -> a -> a
findParent Nil _ = -1
findParent (Node v left right) el = max (helper v left el) (helper v right el)
 where 
     helper _ Nil _ = -1
     helper parent (Node v left right) el 
      |v == el = parent 
      |otherwise = max (helper v left el) (helper v right el)

areCousins :: (Eq a,Num a,Ord a) => BTree a -> a -> a -> Bool
areCousins t a b = if getDepth t a == getDepth t b && findParent t a /= findParent t b then True else False



