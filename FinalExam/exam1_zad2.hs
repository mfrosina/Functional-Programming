main :: IO()
main = do 
    print (prodEvens  [1,2,3,4,5,6])
    print (prodEvens [7.66,7,7.99,7])
    
prodEvens :: (Num a) => [a] -> a
prodEvens xs = foldr (*) 1 [x | (x,i) <- zip xs [0..], even i]