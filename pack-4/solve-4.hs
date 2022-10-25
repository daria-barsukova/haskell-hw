bin:: Int -> [Int]
bin x | x <= 1 = [x]
      | otherwise = bin (x `div` 2) ++ [x `mod` 2]
 
dec:: [Int] -> Int -> Int
dec x system = foldr (\q acc -> (acc * system + q)) (last (reverse x)) (init (reverse x))

str:: String -> Int
str x = read x

func:: Int -> [Int] -> [Int]
func x [] = [x]
func x (y:ys) | x < y = x:y:ys
              | otherwise = y:(func x ys)
sort:: [Int] -> [Int]
sort [] = []
sort (x:xs) = func x (sort xs)

lost:: [Int]->Int
lost m = helper (sort m)
      where helper lst | ((head(tail lst) - head lst) > 1) = (head lst) + 1
                       | otherwise = helper (tail lst)
