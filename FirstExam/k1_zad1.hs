{-Да се дефинира функция primesProd :: Int -> Int, която за дадено неотрицателно
цяло число 𝑥 връща произведението на простите числа, по-малки от √𝑥. Да се
реализира линейно рекурсивен процес.
Примери:
primesProd 12 → 6
primesProd 1200 → 200560490130-}

main :: IO()
main = do
    print (primesProd 12)
primesProd :: Int -> Int
primesProd x = helper x 2 1
 where
     helper k d prod
      |d > floor (sqrt (fromIntegral k)) = prod
      |isPrime d = helper k (d+1) (prod * d)
      |otherwise = helper k (d+1) prod

isPrime :: Int -> Bool
isPrime n = [m | m <- [1..n], n `mod` m == 0] == [1,n]
