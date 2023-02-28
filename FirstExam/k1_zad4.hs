{-Да се дефинира функция findSmallest :: (Num a, Ord a) => [a] -> [a],
която замества всеки елемент от списък с предишния му по-малък елемент.
Предишният по-малък елемент на число 𝑥 от даден списък е последното число от
списъка, което се намира вляво от 𝑥 и е по-малко от 𝑥. Ако такова число не
съществува, 𝑥 да се замести с -1.
Примери:
findSmallest [2, 5, 3, 7, 8, 1, 9] → [-1, 2, 2, 3, 7, -1, 1]
findSmallest [5, 7, 4, 9, 8, 10] → [-1, 5, -1, 4, 4, 8]
findSmallest [1, 5, 2, 2, 2, 5, 5, 4] → [-1, 1, 1, 1, 1, 2, 2, 2]-}

main :: IO()
main = do
    print (findSmallest [2, 5, 3, 7, 8, 1, 9])

findSmallest :: (Num a,Ord a) => [a] -> [a]
findSmallest [] = []
findSmallest [x] = [-1]
findSmallest (x:y:xs)
 |x < y = x : findSmallest (y:xs)
 |otherwise = -1 : findSmallest (y:xs)