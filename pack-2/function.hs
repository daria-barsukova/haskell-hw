head' :: [Int] -> Int
head' [] = error "No"
head' (x:xs) = x

tail' :: [Int] -> [Int]
tail' [] = error "No"
tail' (x:xs) = xs

last' :: [Int] -> Int
last' [] = error "No"
last' (x:[]) = x
last' (x:xs) = last' xs

init' :: [Int] -> [Int]
init' [] = error "No"
init' [_] = []
init' (x:xs)= x:(init' xs)

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

null' :: [Int] -> Bool
null' list | list == [] = True
           | list /= [] = False

drop' :: Int -> [Int] -> [Int]
drop' 0 x = x
drop' n (x:xs) = drop' (n - 1) xs
drop' _ _ = []

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
main = putStrLn $ show $ sum' [1,2,3]

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

elem' :: Int -> [Int] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x = True
               | otherwise = a `elem'` xs

reverse':: [Int]->[Int]
reverse' []=[]
reverse' xs = revh xs []
  where revh [] acc = acc
        revh (x:xs) acc =revh xs(x:acc)
