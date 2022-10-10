fizzbuzz = [if x `mod` 15 == 0 then "FizzBuzz" else if x `mod` 3  == 0 then "Fizz" else if x `mod` 5  == 0 then "Buzz" else show x | x <- [1..]]

dotsInCircle :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)] 
dotsInCircle (x, y) r array = [(x1, y1) | (x1, y1) <- array, (x1-x)^2 + (y1-y)^2 <= (r)^2]

setAnd :: [Int] -> [Int] -> [Int] 
setAnd (x:xs) (y:ys) = [ x | x <- (x:xs), y <- (y:ys), x==y]

summ :: Int -> Int
summ 0 = 0
summ x = (x `mod` 10) + summ (x `div` 10)

count :: Int -> Int
count x | x < 10 = 1 
        | otherwise = 1 + count (div x 10)

task :: Integer -> Bool
task x = if x `elem` take 8 [2^n | n <- [1,2..]] then True else False

sequenceByPred :: (Int -> Int) -> Int -> [Int]
sequenceByPred f start = start : sequenceByPred f second
      where second = f start

sequenceByPred2 :: (Int -> Int -> Int) -> Int -> Int -> [Int]
sequenceByPred2 f start1 start2 = start1 : start2 : sequenceByPred2 f third fourth
      where third = f start1 start2
            fourth = f start2 third

count' :: Int -> Int
count' x = coll 0 x
coll acc 1 = acc
coll acc x = if x `mod` 2 == 0 then coll (acc + 1) (x `div` 2) else coll (acc + 1) (3 * x + 1)

max' :: Int -> Int 
max' x = coll1 0 x 
coll1 acc 1 = acc 
coll1 acc x = if x `mod` 2 == 0 then if x > acc then coll1 x (x `div` 2) else coll1 acc (x `div` 2) else if x > acc then coll1 x (3 * x + 1) else coll1 acc (3 * x + 1)

pow :: Int -> Int
pow k = go 1
      where go 10000 = 10000
            go n     = if (2^n > k) then n else go (n+1)

func :: Int -> [Int] -> [Int]
func x [] = [x]
func x (y:ys) | x < y = x:y:ys
              | otherwise = y:(func x ys)
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = func x (sort xs)
