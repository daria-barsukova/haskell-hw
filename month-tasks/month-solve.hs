inc x | x >= 0 = x + 1
      | otherwise = error "Arg must be positive!"
dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Arg must be positive!"

pls:: Int -> Int -> Int
pls x y | y > 0 = pls (inc x) (dec y)
        | y == 0 = x
        | otherwise = error "Arg must be positive!"

mns:: Int -> Int -> Int
mns x y | y > 0 = mns (dec x) (dec y)
        | y == 0 = x
        | otherwise = error "Arg must be positive!"

mlt:: Int -> Int -> Int
mlt x 1 = x
mlt 1 y = y
mlt x y = pls y (mlt (dec x) (y))

max':: Int -> Int -> Int
max' x y | mns x y == 0 = y
         | otherwise = x

min':: Int -> Int -> Int
min' x y | mns x y == 0 = x
         | otherwise = y

div':: Int -> Int -> Int
div' x y | (y == 0) = error "No"
         | (y < 0) || (x < 0) = error "Arg must be positive!"
         | otherwise = help x y 0 0
help x y acc1 acc2 | (acc2 == x) = acc1
                   | (acc2 > x) = (acc1 - 1)
                   | otherwise = help x y (inc acc1) (pls acc2 y)

div'':: Int -> Int -> Int
div'' x y | (y == 0) = error "No"
          | (y < 0) || (x < 0) = error "Arg must be positive!"
          | otherwise = help2 x y 0
help2 x y acc | (x < y) = acc
              | otherwise = help2 (mns x y) y (inc acc)

mod':: Int -> Int -> Int
mod' x y | (y == 0) = error "No"
         | (y < 0) || (x < 0) = error "Arg must be positive!"
         | otherwise = help3 x y 0 0
help3 x y acc1 acc2 | (acc2 == x) = x - acc1 * y
                    | (acc2 > x) = x - (dec acc1) * y
                    | otherwise = help3 x y (inc acc1) (pls acc2 y)

mod'':: Int -> Int -> Int
mod'' x y | (y == 0) = error "No"
          | (y < 0) || (x < 0) = error "Arg must be positive!"
          | otherwise = help4 x y 0
help4 x y acc | (x < y) = x
              | otherwise = help4 (mns x y) y (inc acc)

pred':: Int -> Int -> Bool
pred' x y | y == 0 = error "No"
          | otherwise = help' x y
help' x y | x == 0 = True
          | x < y =  False
          | otherwise= help' (x - y) y

nd:: Int -> Int
nd x = helper5 x 1 0
    where helper5 x k c | k >= x = c + 1
                        | x `mod` k == 0 = helper5 x (k + 1) (c + 1)
                        | otherwise = helper5 x (k + 1) c

sumd:: Int -> Int
sumd x = helper x 1 0
    where helper x k c | k >= x = c + k
                       | x `mod''` k == 0 = helper x (k + 1) (c + k)
                       | otherwise = helper x (k + 1) c

pred'':: Int -> Bool
pred'' x | length [ n | n <- [2..x], x `mod''` n == 0] == 1 = True
         | otherwise = False

pnd:: Int -> Int
pnd x = help x x 2 0
    where help s x k c | k >= s = c
                       | x `mod''` k == 0 = help s (d x k) (k+1) (c+1)
                       | otherwise = help s x (k+1) c
          d x k | x `mod''` k == 0 = d (x `div''` k) k
                | otherwise = x

nod' :: Int->Int->Int
nod' x 0 = x
nod' x y = nod' y (mod'' x y)

nok'::Int -> Int -> Int
nok' x y = (x*y) `div''` (nod' x y)
