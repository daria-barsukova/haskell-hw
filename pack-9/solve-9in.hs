import Control.Monad.State
import Distribution.Simple.Utils

-- see this program as example:
{-
x=foo
y=bar
y=$x
l=l
x=$y
-}
{-
At the end of this program the state is:
x = foo
y = foo
l = l
-}

exampleProgram :: String
exampleProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$y"

-- one of possible answers. order in list doesn't matter
exampleAns :: [(String, String)]
exampleAns = [("x", "foo"), ("y", "foo"), ("l", "l")]

check :: IO ()
check = do
    let resultState = solveState exampleProgram
    if exampleAns `listEq` resultState
        then putStrLn "OK!"
        else error "something wrong:("
    where listEq l r = leftInRight && rightInLeft
            where leftInRight = all (\x -> x `elem` r) l
                  rightInLeft = all (\x -> x `elem` l) r

data Value = Literal String | VariableReference String deriving (Show)
data Command = Command { varName :: String, whatToPut :: Value } deriving (Show)

-- you can choose something else!
type InterpreterState = [(String, String)]

solveState :: String -> [(String, String)]
solveState input = interpretToState (map parse $ lines input)

-- example: "foo=bar" -> Command "foo" (Literal "bar")
-- example: "foo=$bar" -> Command "foo" (VariableReference "bar")
parse :: String -> Command
parse x = do
    let separation = unintersperse '=' x
    let strData    = head $ tail separation
    let data_ | head strData == '$' = VariableReference $ tail $ strData 
              | otherwise           =  Literal strData
    Command (head separation) data_

-- you may rewrite this. e.g. you can use fold
-- but if you look at standard library there might be
-- a better alternative for chaining state functions.
-- In other words, executing a list of (State s a)
-- functions is a common task, and it has a standard implementation
interpretMany :: [Command] -> State InterpreterState ()
interpretMany [] = return ()
interpretMany (x:xs) = do
    interpretOne x
    interpretMany xs

-- using get, set and other State functions, interpret the command
interpretOne :: Command -> State InterpreterState ()
interpretOne command = do
    stateNow <- get
    let data_ = whatToPut command
    put $ [state | state <- stateNow, fst state /= varName command] ++ [takeCmd command data_ stateNow]
        where takeCmd command (Literal data_) stateNow           = (varName command, data_)
              takeCmd command (VariableReference data_) stateNow = (varName command, res data_ stateNow)
                  where res input ((variable, data_) : state) = if variable == input then data_ else res input state 

-- you can choose other type for result
interpretToState :: [Command] -> [(String, String)]
interpretToState commands = execState (interpretMany commands) emptyState
    where emptyState = []
    