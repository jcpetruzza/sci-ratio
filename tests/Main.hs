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

parseNumberT :: String -> Either String SciRational
parseNumberT s = case P.readP_to_S pNumber s of
  [(x, [])] -> Right x
  r         -> Left $ "can't parse: " ++ s ++ " (" ++ show r ++ ")"

test :: Test ()
test = do

  -- recip must recanonicalize the number otherwise this test would fail
  if 5 /= recip (1 / 5 :: SciRational)
    then failTest "5 /= recip (1 / 5)"
    else passTest "5 == recip (1 / 5)"

  (`mapM_` testCases) $ \ s ->
    case parseNumberT s of
      Left e -> failTest e
      Right x ->
        let s' = prettyNumber (x :: SciRational) in
        case parseNumberT s' of
          Left e -> failTest e
          Right x' ->
            if x' /= x
            then failTest $ "prettied result (" ++ show x' ++
                            ") not equal to original (" ++ show x ++ ")"
            else passTest $ s ++ " ==> " ++ s'

main :: IO ()
main = do
  ((), TestState { failCount = n }) <- runTest test initTestState
  if n > 0 then exitFailure else exitSuccess
