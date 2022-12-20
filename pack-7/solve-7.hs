import System.IO
import Data.ByteString.Lazy
import System.Directory 

-- task 1 Sanity Check
printFile :: String -> IO ()
printFile fileToPrint = do
    handle <- openFile "ex.txt" ReadMode  
    contents <- System.IO.hGetContents handle  
    System.IO.putStr $ contents  ++ "\n"
    hClose handle 

-- task 2 Text Check
areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText file1 file2 = do
    contents1 <- System.IO.readFile file1
    contents2 <- System.IO.readFile file2
    return (contents1 == contents2)
    
-- task 3 Dos2Unix
dos2unix :: FilePath -> IO ()
dos2unix file = do
    content <- System.IO.readFile file
    System.IO.putStr (helper content)
        where helper = Prelude.foldr (\symb acc -> if symb == '\r' then acc else symb : acc) ""

unix2dox :: FilePath -> IO ()
unix2dox file = do
    content <- System.IO.readFile file
    System.IO.putStr (helper content)
        where helper = Prelude.foldr (\symb acc -> if symb == '\n' then '\r' : symb : acc else symb : acc) ""

-- task 4 Binary check
areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin file1 file2 = do
    contents1 <- Data.ByteString.Lazy.readFile file1
    contents2 <- Data.ByteString.Lazy.readFile file2
    return (contents1 == contents2)

-- task 5 No Vimmers?
fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited file = doesFileExist (file ++ ".sw")
