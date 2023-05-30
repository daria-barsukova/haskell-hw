import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Data.Char


--
-- fix this!!!
solveQ :: Double -> Double -> Double -> Maybe (Double, Double) 
solveQ a b c = do
    guard $ b^2 - 4 * a * c >= 0
    d <- Just $ b^2 - 4 * a * c
    return ((-b + sqrt d) / 2 * a, (-b + sqrt d) / 2 * a)

-- fix this code.
solveUserQ :: MaybeT IO (Double, Double)
solveUserQ = do
  input <- lift getLine -- (1,2,3)
  let (a, b, c) = read input :: (Double, Double, Double)
  r <- MaybeT (pure $ solveQ a b c)
  return r


type Log = [String]
data UserInfo = UserInfo { address :: String, name :: String, salary :: Int } deriving Show

makeSureUserIsComfortableGivingInformation :: String -> MaybeT IO ()
makeSureUserIsComfortableGivingInformation infoName = do
    ans <- lift $ putStrLn infoName >> getLine
    lift $ putStrLn $ "User has responded with " ++ ans
    guard $ map toLower ans == "yes"
-- print "Are you ok sharing " ++ infoName
-- get input from user
-- if not equal to "yes", fail (use guard)
-- else do not fail

-- example purposes. This MaybeT function always fails
nothingExample :: MaybeT IO ()
nothingExample = do
    guard False

-- you can run this using runGetUserInfo
getUserInfo :: WriterT Log (MaybeT IO) UserInfo
getUserInfo = do
   -- here is example code. remove it when actually solving
   -- tell "this is log example"
   -- (lift . lift) $ putStrLn "this is IO example"
   -- (liftIO) $ putStrLn "this works, too"
   {- Try uncommenting the next line and running runGetUserInfo -}
   -- r <- lift nothingExample
   -- return $ UserInfo "" "" 12
    address <- getUserAddress
    name    <- getUserName
    salary  <- getUserSalary
    return (UserInfo address name salary)

getUserSalary :: WriterT Log (MaybeT IO) Int
getUserSalary = do
  lift $ makeSureUserIsComfortableGivingInformation "Are you ok sharing your salary?"
  ans <- lift . lift $ putStrLn "Enter your salary " >> getLine
  let salaryAmt = read ans :: Int
  tell ["salary"]
  return salaryAmt
-- ask user if comfortable to share (use makeSureUserIsComfortableGivingInformation)
-- get user answer
-- log what user has typed using WriterR
-- return user answer
  

getUserName :: WriterT Log (MaybeT IO) String
getUserName = do
  lift $ makeSureUserIsComfortableGivingInformation "Are you ok sharing your name?"
  ans <- lift . lift $ putStrLn "Enter your name " >> getLine
  tell ["name"]
  return ans
-- same as getUserSalary

getUserAddress :: WriterT Log (MaybeT IO) String
getUserAddress = do
  lift $ makeSureUserIsComfortableGivingInformation "Are you ok sharing your address?"
  ans <- lift . lift $ putStrLn "Enter your adress " >> getLine
  tell ["address"]
  return ans
-- same as getUserSalary


runGetUserInfo :: IO ()
runGetUserInfo = do
    res <- runMaybeT . runWriterT $ getUserInfo
    print res
