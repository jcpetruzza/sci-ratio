module Main where
import Control.Applicative
  ( Applicative((<*>), pure)
  , Alternative((<|>), empty) )
import Control.Monad
  ( ap, MonadPlus(mplus, mzero) )
import System.Exit
import qualified Text.ParserCombinators.ReadP as P
import Data.SciRatio
import Data.SciRatio.Utils

data TestState
  = TestState
    { failCount :: Int }

initTestState
  = TestState
    { failCount = 0 }

newtype Test a = Test { runTest :: TestState -> IO (a, TestState) }

instance Functor Test where
  fmap f u = return f `ap` u

instance Applicative Test where
  pure  = return
  (<*>) = ap

instance Monad Test where
  return x     = Test $ \ s -> return (x, s)
  Test f >>= u = Test $ \ s -> do
    (x, s') <- f s
    runTest (u x) s'
  fail s       = failTest "aborted" >> fail s

failTest :: String -> Test ()
failTest s = do
  liftIO . putStrLn $ "*** FAIL: " ++ s
  s <- getTestState
  let TestState { failCount = n } = s
  putTestState s { failCount = n + 1 }

passTest :: String -> Test ()
passTest s =
  liftIO . putStrLn $ "ok" ++ if null s then "" else " (" ++ s ++ ")"

expect :: String -> Bool -> Test ()
expect s False = failTest s
expect s True  = passTest s

liftIO :: IO a -> Test a
liftIO m = Test $ \ s -> do
  x <- m
  return (x, s)

getTestState :: Test TestState
getTestState = Test $ \ s -> return (s, s)

putTestState :: TestState -> Test ()
putTestState s = Test $ \ _ -> return ((), s)

testCases :: [String]
testCases =
  [ "0"
  , "-00"
  , "-.0000"
  , "-.01"
  , ".00008"
  , "1"
  , "+1"
  , "-1"
  , "1."
  , "-1."
  , "+1."
  , "3/5"
  , "5e20/5"
  , "1/3"
  , "-2/3"
  , "-2000/3"
  , "37e-200/125"
  , "-1/2"
  , "2e1"
  , "1e2"
  , "1e-3"
  , "-.48e+1/.39"
  , "-2.14e+1/17"
  , "-.3e+1/17"
  , "-2.e+1/17"
  , "3.14e-17"
  , "0.23456e-99999990"
  , "-0.002397/338792e+9999999990"
  ]

parseNumberT :: String -> (SciRational -> Test ()) -> Test ()
parseNumberT s f = case P.readP_to_S pNumber s of
  [(x, [])] -> f x
  r         -> failTest $ "can't parse: " ++ s ++ " (" ++ show r ++ ")"

-- | Tests the comparison operation in both directions.
testOrd :: String -> Ordering -> String -> Test ()
testOrd s ordering s' =
  parseNumberT s  $ \ x  ->
  parseNumberT s' $ \ x' -> do
  expect (s ++ " " ++ show ordering ++ " " ++ s') $
         compare x x' == ordering
  expect (s' ++ " " ++ show ordering' ++ " " ++ s) $
         compare x' x == ordering'
  where invert GT = LT
        invert EQ = EQ
        invert LT = GT
        ordering' = invert ordering

test :: Test ()
test = do

  -- recip must recanonicalize the number otherwise this test would fail
  expect "5 == recip (1 / 5)" $
          5 == recip (1 / 5 :: SciRational)

  -- comparison tests
  testOrd "0" EQ "0"
  testOrd "0" LT "1"
  testOrd "0" GT "-1"
  testOrd "-5e2" LT "400"
  testOrd "5e2" GT "400"
  testOrd "3.5e99999" GT "-2"
  testOrd "-3.5e99999" LT "2"
  testOrd "-3.5e-99999" LT "2"
  testOrd "3.5e99999" EQ "3.5e99999"
  testOrd "-3.5e99999" LT "3.5e99999"
  testOrd "-3.5e99999" EQ "-3.5e99999"
  testOrd "-3.5e99999" LT "-2.5e99999"
  testOrd "3.5e99999" GT "2.5e99999"
  testOrd "3.5e99999" GT "0"

  (`mapM_` testCases) $ \ s ->
    parseNumberT s $ \ x ->
    let s' = prettyNumber (x :: SciRational) in
    parseNumberT s' $ \ x' ->
    if x' /= x
    then failTest $ "prettied result (" ++ show x' ++
                    ") not equal to original (" ++ show x ++ ")"
    else passTest $ s ++ " ==> " ++ s'

main :: IO ()
main = do
  ((), TestState { failCount = n }) <- runTest test initTestState
  if n > 0 then exitFailure else exitSuccess
