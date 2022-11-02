import Data.Char
bin:: Int -> [Int]
bin x | x <= 1 = [x]
      | otherwise = bin (x `div` 2) ++ [x `mod` 2]
 
dec:: [Int] -> Int -> Int
dec x system = foldr (\q acc -> (acc * system + q)) (last (reverse x)) (init (reverse x))

str:: String -> Int
str = helper 0
    where helper c [] = c
          helper c (x:xs) | 0 <= ch && ch <= 9 = helper (10 * c + ch) xs
                          | otherwise = 0
              where ch = digitToInt x

func:: Int -> [Int] -> [Int]
func x [] = [x]
func x (y:ys) | x < y = x:y:ys
              | otherwise = y:(func x ys)
sort:: [Int] -> [Int]
sort [] = []
sort (x:xs) = func x (sort xs)

lost:: [Int]->Int
lost m = helper (sort m)
      where helper lst | (z - y > 1) = y + 1
                       | otherwise = helper x
                  where x = tail lst 
                        y = head lst
                        z = head(x)
