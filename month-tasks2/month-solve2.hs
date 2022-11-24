
import Prelude hiding ((!!),init,reverse,(++),cycle,take,elem)

--1
count :: [a] -> Int -> a
count x y | length x == 0 = error "NO"
          | y == 0 = head x
          | otherwise = count (tail x) (y - 1)
-- count [1,2,3] 0 == 1

--2
init' :: [a] -> [a]
init' x | length x == 0 = error "NO"
        | length x == 1 = []
        | otherwise = (head x) : init' (tail x)
-- init' [1,2,3] == [1,2]

--3
concat' :: [a] -> [a] -> [a]
concat' [] y = y
concat' x y = (head x) : (concat' (tail x) y)
-- concat' [1,2,3] [2,3,3,4] == [1,2,3,3,4,5,2,1]

--4
cycle' :: [a] -> [a]
cycle' x = concat' x (cycle' x)
-- cycle' [1,2,3]

--5
take' :: Int -> [a] -> [a]
take' x _ | x <= 0 = []
take' x y = concat' (take' (x-1) y) ([count y (x-1)])
-- take' 5 [1..] == [1,2,3,4,5]

--6
inits :: [a] -> [[a]]
inits x = concat' [[]] [take' z x| z <- [1..(length x)]]
-- inits [1,2,3] == [[],[1],[1,2],[1,2,3]]

tails :: [a] -> [[a]]
tails x = concat' [drop (z - 1) x| z <- [1..(length x)]] [[]]
-- tails [1,2,3] == [[1,2,3],[2,3],[3],[]]

--7
elem' :: Eq a => a -> [a] -> Bool
elem' x y | length ([z | z <- y, x == z]) > 0 = True
          | otherwise = False
-- elem' 1 [1,2,3] == True

--8
nub :: Eq a => [a] -> [a]
nub x | length x == 0 = []
      | head x `notElem` (tail x) = head x : nub (tail x) 
      | otherwise = nub (tail x)
-- nub [1,2,2,2] == [1,2]

--9
updElmBy :: [a] -> Int -> a -> [a]
updElmBy b c d | (length b == 0) || (c < 0) = error "NO"
               | c == 0 = concat' [d] (tail b)
               | otherwise = (head b) : updElmBy (tail b) (c - 1) d
-- updElmBy [1,2,3,4] 3 44 == [1,2,3,44]

-- N10
swp :: [a] -> Int -> Int -> [a]
swp [] _ _ = []
swp x y z | y > z = concat' (concat' (concat' (concat' (take' z x) ([count x y])) (drop (z+1) (take' y x))) ([count x z])) (drop (y+1) x)
          | otherwise = concat' (concat' (concat' (concat' (take' y x) ([count x z])) (drop (y+1) (take' z x))) ([count x y])) (drop (z+1) x)
-- swp [1,2,3,4] 1 2 == [1,3,2,4]

-- 11
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations x = [ y:z | y <- x, z <- (permutations [t | t <- x, t /= y])]
-- permutations [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

-- 12
subsequences :: Eq a => [a] -> [[a]]
subsequences [] = [[]]
subsequences x = [] : [y:z | f <- [0..(length x)], y <- (take' f x), z <- (permutations [t | t <- (take' f x), t /= y])]
-- subsequences [1,2,2,3] == [[],[1],[2],[1,2],[2],[1,2],[2,2],[1,2,2],[3],[1,3],[2,3],[1,2,3],[2,3],[1,2,3],[2,2,3],[1,2,2,3]]

-- 13
cubsum :: [Int] -> Int
cubsum [] = 0
cubsum xs = foldr (\x y -> y + x*x*x) 0 xs
-- cubsum [1,2,3] == 36

-- 14
cubsum' :: [Int] -> Int
cubsum' [] = 0
cubsum' xs = foldl (\y x -> y + x*x*x) 0 xs
-- cubsum' [1,2,3] == 36

-- 15
fact :: Int -> Int
fact x = foldl (*) 1 [1..x]

expT :: Double -> Int -> Double
expT y x = foldr (+) 1.0 [y^^w / (fromIntegral (fact w))| w <- [1..x]]
-- expT 1 5 == 2.716666666666667

-- 16
howmany :: Eq a => a -> [a] -> Int
howmany x y = foldr (+) 0 [1 | z <- y, z == x]
-- howmany 'l' "hello" == 2

-- 17
howmany_g_b_letters :: [Char] -> (Int, Int)
howmany_g_b_letters x = (sum [howmany a x | a <- "aeiou", elem' a x], sum [howmany a x | a <- "tnshr", elem' a x])
-- howmany_g_b_letters "hello" == (2,1)

-- 18
intersperse :: a -> [a] -> [a]
intersperse xs ys = take' (length ys * 2 - 1) (foldr (\x y -> x : xs : y) [] ys)
-- intersperse ',' "hello" == "h,e,l,l,o"

-- 19
rotate :: [a] -> [[a]]
rotate x = [concat' (drop (length x - z) x) (take' (length x - z) x) | z <- [1..(length x)]]
-- rotate [1,2,3,4] == [[4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]]