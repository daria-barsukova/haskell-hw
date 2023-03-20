import Data.Ord
import Data.Char


-- example:
-- makeArt 0 == ""
-- makeArt 1
-- wil result in:
-- a
-- makeArt 3
{-
**a**
*b*b*
c***c
*b*b*
**a**
-}

makeDrawing:: Int -> Int -> String 
makeDrawing line numb = [if srt == numb - line + 1 + 2 * (line - numb) * fromEnum(line > numb) ||
                            srt == numb + line - 1 - 2 * (line - numb) * fromEnum(line > numb)
                         then chr(ord('a') + line - 1 - 2 * (line - numb) * fromEnum(line > numb)) 
                         else '*' 
                         | srt <- [1..(2 * numb - 1)]]

task:: Int -> String
task numb = foldl (\a line -> a ++ (makeDrawing line numb ) ++ "\n") "" [1..(2 * numb - 1)]

makeArt :: Int -> String
makeArt numb | numb == 0 = ""
             | otherwise = task numb

main :: IO ()
main = putStrLn $ makeArt 3
