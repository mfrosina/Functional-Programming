main :: IO()
main = do
    print (findNb 1071225)
    print (findNb 40539911473216) -- 3568
    --print (findNb 135440716410000) 
    --print (findNb 4183059834009) 
     --print (findNb 91716553919377)
    --print (findNb 24723578342962)

formula :: Integer -> Integer 
formula 0 = 0
formula n = (n^3) + formula (n-1)

findNb :: Integer -> Integer 
findNb 0 = 0
findNb m = helper m 0
 where 
     helper m n 
      |(formula n) > m = -1
      |(formula n) == m = n 
      |otherwise = helper m (n + 1)
