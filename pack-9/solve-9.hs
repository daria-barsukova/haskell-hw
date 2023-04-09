import Prelude hiding (head, tail, maximum)
import Data.Maybe
import Control.Monad.State

type GreekData = [(String, [Integer])]
greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 60, 47])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191,63])
             , ("psi", [])
             , ("omega", [6, 82, 144])
             ]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

divMay :: Double -> Double -> Maybe Double
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (foldr max x xs)

tailMay :: Ord a => [a] -> Maybe a
tailMay [] = Nothing
tailMay (x:xs) = maximumMay xs


{-
 tl;dr implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in

 first query the GreekData that is passed in,
 look up the string passed in the second argument,
 and retrieve the corresponding list of Integers. Call this list xs.
 Next calculate the maximum of the tail of xs
 (Don’t use any pattern matching here.
 Use case expressions and the maximumMay and tailMay functions)
 Take the maximum and divide it by the head of the list (using headMay and divMay functions).
 If any of these operations along the way return Nothing, then your function should return Nothing.
 But if everything succeeds, then return the final quotient.
 One hint… you’ll need to use the fromIntegral function to convert your two Integers to Doubles for the final call to divMay.
-}


queryGreek :: GreekData -> String -> Maybe Double
queryGreek gdata value = helper (fromJust (lookup value gdata))
    where helper xs = divMay (fromIntegral (fromJust (tailMay xs))) (fromIntegral (fromJust (headMay xs)))

-- queryGreek greekDataA "alpha" == Just 2.0

-- Now do the same whole thing, but using do-notation, since Maybe is a Monad
queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro gdata value = do
    xs <- lookup value gdata
    divisible <- tailMay xs
    divider <- headMay xs
    ans <- divMay (fromIntegral divisible) (fromIntegral divider)
    return ans

-- * a harder task. rewrite queryGreekPro, but without the do-notation, only using the (>>=) operator and its friends
-- in other words, desugarize your notation
queryGreekProPlus :: GreekData -> String -> Maybe Double
queryGreekProPlus gdata value = (lookup value gdata) >>= (\a -> tailMay a >>= (\b -> headMay a >>= (\c -> divMay (fromIntegral b) (fromIntegral c))))


-- state monad
type RandState = Int

a = 1664525 
c = 1013904223

rollDice :: State RandState Int
rollDice = do
    seed <- get
    let res = mod (a * seed + c) 6
    put res
    return res

game :: State RandState String
game = do
    firstPlayerRes <- rollDice
    secondPlayerRes <- rollDice
    if firstPlayerRes > secondPlayerRes then return "First wins" else return "Second wins"

runGame :: String
runGame = evalState game startSeed
    where startSeed = 4
