type Variables = (Int, Int, Int)
data MonadVars a = MonadVars { actualComputation :: (Variables -> (a, Variables)) }

runComputation :: MonadVars a -> Variables -> (a, Variables)
runComputation computation vars = actualComputation computation $ vars

getVars :: MonadVars Variables
getVars = MonadVars $ \vars -> (vars, vars)

instance Functor MonadVars where
    -- fmap :: (a -> b) -> (MonadVars a) -> (MonadVars b)
    fmap f (MonadVars comp) =
        MonadVars $ \vars -> let (result, newVars) = comp vars in (f result, newVars)

instance Applicative MonadVars where
    pure x = MonadVars $ \vars -> (x, vars)
    (MonadVars compF) <*> (MonadVars compX) = 
        MonadVars $ \vars -> 
        let (f, newVars) = compF vars
            (x, finalVars) = compX newVars
        in (f x, finalVars)

instance Monad MonadVars where
    (comp1) >>= (comp2) =
        MonadVars $ \vars ->
        let (res1,newVars) = runComputation comp1 vars
        in runComputation (comp2 res1) newVars

putVars :: Variables -> MonadVars ()
putVars var = MonadVars $ \vars -> ((), var)

computation :: MonadVars Int
computation = do
    (x1, x2, x3) <- getVars
    putVars (x1 + 2, x2 + 1, x3 * 3)
    return 200

main :: IO ()
main = do
    print $ runComputation computation (20, 30, 40)
