main :: IO()
main = do
    print(isBoring t1)
    print (isBoring t2)
    
data NTree a = NullT | Node a (NTree a)(NTree a)
 deriving Show

t1 ::(Num a) => NTree a
t1 = Node 6 (Node 6 (Node 6 NullT NullT) NullT) (Node 6 NullT NullT)

t2 :: NTree Char
t2 = Node 'a' (Node 'c' (Node 'f' NullT NullT)NullT) (Node 'd' NullT NullT)

treeToList :: NTree a -> [a]
treeToList NullT = []
treeToList (Node v l r) = v : (treeToList l) ++ (treeToList r)

isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node _ NullT NullT) = True
isBoring t@(Node v left right) = helper $ treeToList t
 where
     helper :: (Eq a) => [a] -> Bool
     helper [] = True
     helper [x] = True
     helper (x:y:xs) = x == y && helper (y:xs)
