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
       , Dog "Iacchus" 4 Female Beagle True
       , Dog "Socrates" 5 Male GoldenRetrievers True
       , Dog "Zeus" 5 Male Beagle True
       , Dog "Fiona" 4 Female Terrier True
       , Dog "Aisyah" 5 Female Beagle True] 


{-
-- examples
-- how many dogs are of age 2, 4 and 6?
dogsAge246 :: [Dog]
dogsAge246 = do
	dogsAge2 <- dogsAge 2
	dogsAge4 <- dogsAge 4
	dogsAge6 <- dogsAge 6
	return $ (dogsAge2, dogsAge4, dogsAge6)-}


-- using do-notation, find such dogs, that they are male, more than 3 years old, not IrishSetter, good boys
-- and such dogs, that they are female, more than 3 years old, name is longer then 4 symbols
-- after finding those two groups, combine a list of all combinations they could be mated

male :: [Dog]
male = filter (\(Dog name age gender breed isGoodBoy) -> (age > 3) && (breed /= IrishSetter) && (gender == Male) && (isGoodBoy == True)) dogs

female :: [Dog]
female = filter (\(Dog name age gender breed isGoodBoy) -> (age > 3) && (gender == Female) && (length (name)) > 4) dogs

comb :: [Dog] -> [Dog] -> [Int] -> [[(Dog, Dog)]]
comb _ _ [] = []
comb (x:xs) (y:ys) (a:b) = [((x, y) : (helperComb xs ys))] ++ comb (xs ++ [x]) (y:ys) b
  where helperComb [] _ = []
        helperComb _ [] = []
        helperComb (x:xs) (y:ys) = ((x, y) : (helperComb xs ys))

dogsQuery :: [[(Dog, Dog)]]
dogsQuery = do
    comb male female [1..fact (max (length male) (length female))]
      where fact 1 = 1
            fact b = b * fact (b - 1)

printf :: [[(Dog, Dog)]] -> [[(String, String)]]
printf dogs = [[(name d1, name d2) | (d1, d2) <- pair] | pair <- dogs]


prettyBoard :: (Int, Int) -> (Int, Int) -> String
prettyBoard (x1, y1) (x2, y2) = foldl(\acc temp -> acc ++ (p temp) ++ "\n") [] [0..7]
  where p line | line == x1 = foldl(\acc y -> if y == y1 then acc ++ "Q " else acc ++ "_ ") "" [0..7]
               | line == x2 = foldl(\acc y -> if y == y2 then acc ++ "q " else acc ++ "_ ") "" [0..7]
               | otherwise = "_ _ _ _ _ _ _ _ "

main :: IO ()
main = putStrLn $ prettyBoard (3,7) (4,2)

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) | x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2) = True
                            | otherwise = False
