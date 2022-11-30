quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c | d < 0 = Nothing
                      | d > 0 = Just $ (x1,x2)
                      | otherwise = Just $ (x,x)
                        where 
                          d = b^2 - 4 * a * c
                          sd = sqrt d
                          x1 = (-b - sd) / (2 * a)
                          x2 = (-b + sd) / (2 * a)
                          x = - c / b

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just $ x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (x:xs) = Just $ xs

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit (x:xs) = Just $ maybeInit' x xs
  where maybeInit' _ [] = []
        maybeInit' y (x:xs) = y : maybeInit' x xs

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ [] = Nothing
maybeFind predicate (x:xs) | predicate x = Just $ x
                           | otherwise = maybeFind predicate xs

data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool -- holds for female dogs as well
               } deriving (Show, Eq)

dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

goodBoys :: [Dog]
goodBoys = [x | x <- dogs, isGoodBoy $ x]

longNamedDogs :: [Dog]
longNamedDogs = [x | x <- dogs, (length ( name $ x) > 7)]

mostPopularDogGender :: Gender
mostPopularDogGender = if female > male then Female else Male
  where female = length $ filter (\x -> gender x == Female) dogs
        summ = length dogs
        male = summ - female

oldestDog :: Dog
oldestDog = head $ filter (\x -> age x == max) dogs
  where max = maximum $ map age dogs

averageDogAge :: Double
averageDogAge = (fromIntegral $ sum age') / (fromIntegral $ length age')
  where age' = map age dogs

dogsByBreed :: DogBreed -> [Dog]
dogsByBreed need = filter (\x -> breed x == need) dogs

data Complex = Complex { re:: Float
                       , im:: Float
                       } deriving (Eq, Show)

summa:: Complex -> Complex -> Complex
summa (Complex re1 im1) (Complex re2 im2) = Complex (re1 + re2) (im1 + im2)

minus:: Complex -> Complex -> Complex
minus (Complex rl il) (Complex rr ir) = Complex (rl - rr) (il - ir)

prod::  Complex -> Complex -> Complex
prod (Complex a b) (Complex c d) = Complex (a * c - b * d) (a * d + c * b) 

del::  Complex -> Complex -> Complex
del (Complex a2 b2) (Complex a1 b1) = Complex ((a1 * a2 + b1 * b2) / (a1 * a1 + b1 * b1)) ((a1 * b2 - a2 * b1) / (a1 * a1 + b1 * b1)) 

sopr::  Complex -> Complex
sopr (Complex a b) = Complex (a) (-b) 

abss:: Complex -> Complex
abss (Complex r i) = Complex (sqrt (r * r + i * i)) 0

data MyList a = Empty | Cons a (MyList a)
 deriving (Show)

fromList :: [a] -> MyList a
fromList [] = Empty
fromList x = Cons (head x) (fromList (tail x))

toList :: MyList a -> [a]
toList Empty = []
toList (Cons x y) = (x : (toList y))

reverseMyList :: MyList a -> MyList a
reverseMyList Empty = Empty
reverseMyList x = helper x Empty
    where helper Empty acc = acc
          helper (Cons x y) acc = helper y (Cons x acc)

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList a Empty = Empty
mapMyList a (Cons x y) = Cons (a x) (mapMyList a y)
