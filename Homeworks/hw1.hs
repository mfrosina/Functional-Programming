{-1.Да се дефинира функция sumCountsIter :: Int -> Int -> Int, която за дадени
естествено число x и цяло число d, 0 ≤ d ≤ 9, връща сбора на цифрите на броя
срещания на цифрата d в числата от интервала [1,x]. Функцията да реализира
линейно итеративен процес. При подаден невалиден вход да се връща грешка с
подходящ текст.
Примери:
sumCountsIter 1 1 ➝ 1
sumCountsIter 5123 1 ➝ 19
2.Да се дефинира функция maxRotation :: Int -> Int, която получава
неотрицателно цяло число и връща максималното цяло число, което се получава при
извършването на всички възможни ротации. При подаден невалиден вход да се връща
грешка с подходящ текст.
Примери:
maxRotation 56789 ➝ 68957 -}

main::IO()
main = do
    print (sumCountsIter 5123 1)
    print (maxRotation 56789)
--1 zad.
countOccurences :: Int -> Int -> Int -> Int
countOccurences count k d 
  |k == 0 = count
  |k`mod`10 == d = countOccurences(count+1) (k`div`10) d
  |otherwise = countOccurences (count) (k`div`10) d

countOccurencesHelper :: Int -> Int -> Int -> Int -> Int
countOccurencesHelper i x d counter = if i == x then counter + countOccurences 0 i d
                                  else countOccurencesHelper (i+1) x d (counter + countOccurences 0 (i+1) d)

numberOfOccurences :: Int -> Int -> Int
numberOfOccurences 1 1 = 1
numberOfOccurences x d = countOccurencesHelper 1 x d 0

sumCountsIter :: Int -> Int -> Int
sumCountsIter x d = if d < 0 || d > 9 then error "d must be in the interval [0..9]" else helper (numberOfOccurences x d) 0
 where
  helper k sum = if k < 10 then sum + k else helper (k`div`10) (sum + k`mod`10)

--2 zad.
countDigits :: Int -> Int
countDigits n = if n < 10 then 1 else 1 + countDigits (n`div`10)

firstDigit :: Int -> Int
firstDigit n = if n < 10 then n else firstDigit (n`div`10)

rotate :: Int -> Int
rotate n = (n * 10 + firstDigit n) - (firstDigit n * pow * 10)
 where pow = 10^((countDigits n) - 1)

allRotations :: Int -> [Int]
allRotations n = helper n 0 (countDigits n)
 where
  helper k i d = if i > ((countDigits k) - 1) then []
                 else k : helper  ((k`div`10^d) * (10^d) + rotate(k`mod`(10^d))) (i+1) (d-1)
                                         

maxRotationHelper :: [Int] -> Int
maxRotationHelper [x] = x
maxRotationHelper (x:x1:xs)
 |x >= x1 = maxRotationHelper (x:xs)
 |otherwise = maxRotationHelper (x1:xs)

maxRotation :: Int -> Int
maxRotation n = maxRotationHelper (allRotations n)
 