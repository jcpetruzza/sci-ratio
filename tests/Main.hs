module Main where
import Data.IORef
import System.Exit
import qualified Text.ParserCombinators.ReadP as P
import Data.SciRatio
import Data.SciRatio.Utils

testCases :: [String]
testCases =
  [ "0"
  , "-00"
  , "-.0000"
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
  , "-2.14e+1/17"
  , "-.3e+1/17"
  , "-2.e+1/17"
  , "3.14e-17"
  , "0.23456e-99999990"
  , "-0.002397/338792e+9999999990"
  ]

parseNumberT :: String -> IO SciRational
parseNumberT s = case P.readP_to_S pNumber s of
  [(x, [])] -> return x
  r         -> error $ "can't parse: " ++ s ++ " (" ++ show r ++ ")"

main :: IO ()
main = do
  failCounter <- newIORef (0 :: Int)
  let failTest s = do
        putStrLn $ "*** FAIL: " ++ s
        modifyIORef failCounter (+ 1)
      passTest s = if null s
             then putStrLn $ "ok"
             else putStrLn $ "ok (" ++ s ++ ")"

  -- recip must recanonicalize the number otherwise this test would fail
  if 5 /= recip (1 / 5 :: SciRational)
    then failTest $ "5 /= recip (1 / 5)"
    else passTest $ "5 == recip (1 / 5)"

  (`mapM_` testCases) $ \ s -> do
    x  <- parseNumberT s
    let s' = prettyNumber (x :: SciRational)
    x' <- parseNumberT s'
    if x' /= x
      then failTest $ "prettied result (" ++ show x' ++
                    ") not equal to original (" ++ show x ++ ")"
      else passTest $ s ++ " ==> " ++ s'

  failCount <- readIORef failCounter
  if failCount > 0 then exitFailure else exitSuccess
