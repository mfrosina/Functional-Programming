main :: IO()
main = do 
    print(maxSumSubT t)
data BTree a = NullT | Node a (BTree a)(BTree a)
 deriving Show 

t :: (Num a) => BTree a 
t = Node 3 ((Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT)NullT))

sumT :: (Ord a, Num a) => BTree a -> Int 
sumT NullT = 0
sumT (Node v l r) = v + (sumT l)(sumT r)

treeToList :: BTree a -> [a]
treeToList NullT = []
treeToList (Node v l r) = v : (treeToList l) ++ (treeToList r)

maxSumSubT :: (Ord a, Num a) => BTree a -> a 
maxSumSubT NullT = 0
maxSumSubT t@(Node v l r) = helper (sumT t treeToList t)
 where
     helper :: (Ord a, Num a,Eq a) => Int -> [a] -> Int
     helper _ [] = 0
     helper s (t:ts)
      |s == sum ts = s 
      |s < t = t 
      |otherwise = s 