{-Да се дефинира функция maxChain :: [(Int, Int)] -> Int, която получава
списък от двойки цели числа и връща дължината на най-дългата верига
(последователност от връзки), която може да бъде оформена. Две двойки могат да
създадат връзка, ако второто число в първата двойка е по-малко от първото число във
втората двойка.
Забележка 1. Двойките числа могат да се използват в произволен ред.
Забележка 2. Приемаме, че във всяка двойка първото число винаги ще бъде по-малко
от второто число.
Примери:
maxChain [(3,4), (5,6), (7,8)] → 3
maxChain [(9,14), (3,5), (4,7)] → 2
Подсказка. За да генерирате всички възможни двойки от числа, използвайте
функцията permutations от библиотеката Data.List.-}
import Data.List
main :: IO()
main = do 
    print (nub $ permutations [(3,4), (5,6), (7,8)])
--maxChain :: [(Int,Int)] -> Int 
