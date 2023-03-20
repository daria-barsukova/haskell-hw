module Barans (
Sheep,
names,
father,
mother
) where


import Control.Monad
import Data.List
import Data.Maybe

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
type TreeList a = [Tree a]

fringe (Leaf x) = [x]
fringe (Branch left _ right) = fringe left ++ fringe right

kolvo :: Tree a -> Int
kolvo (Leaf _ ) = 1
kolvo (Branch l _ r) = kolvo l + kolvo r + 1

leftA :: Tree a -> Maybe (Tree a)
leftA (Leaf _)       = Nothing
leftA (Branch l _ r) = Just l

rightA :: Tree a -> Maybe (Tree a)
rightA (Leaf _)       = Nothing
rightA (Branch l _ r) = Just r

content :: Tree a -> a
content (Leaf x)       = x
content (Branch _ x _) = x

type Sheep = String

mother' :: Sheep -> Tree Sheep -> Maybe Sheep
mother' _ (Leaf _) = Nothing
mother' sheep (Branch l c r) = if (sheep == c) 
                            then Just (content l)  
                            else if (mother' sheep l) == Nothing then mother' sheep r else mother' sheep l

mother'' :: Sheep -> TreeList Sheep -> Maybe Sheep 
mother'' _ [] = Nothing
mother'' sheep (x:xs) = (mother' sheep x) `mplus` mother'' sheep xs

mother sheep = mother'' sheep [i10, i12]

father' :: Sheep -> Tree Sheep -> Maybe Sheep
father' _ (Leaf _) = Nothing
father' sheep (Branch l c r) = if (sheep == c) 
                            then Just (content r)  
                            else (father' sheep l) `mplus` (father' sheep r)

father'' :: Sheep -> TreeList Sheep -> Maybe Sheep 
father'' _ [] = Nothing
father'' sheep (x:xs) = (father' sheep x) `mplus` (father'' sheep xs)

father sheep = father'' sheep [i10, i12]

names' :: Tree Sheep -> [Sheep]
names' (Leaf x)       = [x]
names' (Branch l x r) = (names' l) ++ [x] ++ (names' r)

names'' :: TreeList Sheep -> [Sheep]
names'' [] = []
names'' (x:xs) = (names' x) `mplus` (names'' xs)

names = (sort . nub . names'') [i10, i12]

i8  = Branch (Branch (Leaf "i1") "i3" (Leaf "i2")) "i8" (Leaf "i7")
i9  = Branch (Leaf "i3") "i9" (Leaf "i5")
i10 = Branch i8 "i10" i9
i11 = Branch i8 "i11" i9
i6  = Branch (Leaf "i4") "i6" (Leaf "i5")
i12 = Branch i11 "i12" i6

{--
                      i12
          i10, i11
      i8           i9         i6
   i3    i7     i3   i5    i4    i5
i1    i2

--}


maternal_grandfather:: Sheep -> Maybe Sheep
maternal_grandfather sheep = mother sheep >>= father

maternal_grandfather2:: Sheep -> Maybe Sheep
maternal_grandfather2 sheep = maternal_grandfather sheep >>= father

parents:: Sheep -> [Maybe Sheep]
parents sheep = [mother sheep, father sheep]

grandparents:: Sheep -> [Maybe Sheep]
grandparents sheep | father sheep == Nothing && mother sheep == Nothing = [Nothing, Nothing, Nothing, Nothing]
                   | father sheep == Nothing = parents (fromJust $ mother sheep) ++ [Nothing, Nothing]
                   | mother sheep == Nothing = parents (fromJust $ father sheep) ++ [Nothing, Nothing]
                   | otherwise = parents (fromJust $ mother sheep) ++ parents (fromJust $ father sheep)

isAnOrphan:: Sheep -> Bool
isAnOrphan sheep = if mother sheep == Nothing && father sheep == Nothing
                   then True
                   else False

selected_barans = ["i3", "i5", "i6", "i9", "i12"]

selectedFather:: Sheep -> Maybe Sheep
selectedFather sheep = do
    a <- father sheep
    if (elem a selected_barans) == True
    then return a 
    else Nothing

nearestMale:: Sheep -> Maybe Sheep
nearestMale sheep | father sheep == Nothing = Nothing
                  | selectedFather sheep /= Nothing = selectedFather sheep
                  | otherwise = father sheep >>= nearestMale 
