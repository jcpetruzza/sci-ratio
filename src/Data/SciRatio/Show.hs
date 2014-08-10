{-| Stability: experimental

The functions here pretty-print numbers in a compact format.  Examples:

>>> showSciRational (-0.0e+3)         -- result: "0"
>>> showSciRational (0.25e+2)         -- result: "25"
>>> showSciRational (-1.0e-1)         -- result: "-.1"
>>> showSciRational (5.0e+20 / 6)     -- result: "2.5e20/3"
>>> showSciRational (0xfeedface)      -- result: "4277009102"
>>> showSciRational (1 .^ 99999999)   -- result: "1e99999999"

__Note__: Without taking optimizations into account, the specialized functions
          (@'showSciRational'@ and @'showsSciRational'@) are much more
          efficient than the generic functions (@'showNumber'@ and
          @'showsNumber'@ respectively).

-}
module Data.SciRatio.Show
       (

         -- * Simple pretty-printers
         showNumber
       , showSciRational

         -- * @'ShowS'@ pretty-printers
       , showsNumber
       , showsSciRational

       ) where
import Data.Ratio (denominator, numerator)
import Data.SciRatio (SciRational, base10Exponent, fracSignificand)

-- Note: we need to specialize showNumber and showsNumber in order for the
--       rewrite rules in SciRatio to fire.

-- | Show a number (see @'showsNumber'@).
--
--   Note: for @'SciRational'@, consider using the more efficient, specialized
--         function @'showSciRational'@ instead.
{-# SPECIALIZE showNumber :: SciRational -> String #-}
showNumber :: Real a => a -> String
showNumber x = showsNumber x ""

-- | Show a number (see @'showsNumber'@).
showSciRational :: SciRational -> String
showSciRational x = showsSciRational x ""

-- | Show a rational number in scientific notation:
--
--   > [-+]?
--   > ( [0-9]+ [.]? [0-9]* | [.] [0-9]+ )
--   > ( [e] [-+]? [0-9]+ )?
--   > ( [/] [0-9]+ )?
--
--   Note: for @'SciRational'@, consider using the more efficient, specialized
--         function @'showsSciRational'@ instead.
{-# SPECIALIZE showsNumber :: SciRational -> ShowS #-}
showsNumber :: Real a => a -> ShowS
showsNumber = showsSciRational . realToFrac

-- | Show a number (see @'showNumber'@).
showsSciRational :: SciRational -> ShowS
showsSciRational x =
  let r = toRational (fracSignificand x)
      e = toInteger (base10Exponent x)
      n = numerator r
      d = denominator r in
  -- canonicity ensures that the divisor is not a multiple of 2 nor 5
  case d of
    1 -> showsScientific n e
    _ -> showsFraction n e d

-- | Same as @'shows'@ but specialized to @'Integer'@.
showsInteger :: Integer -> ShowS
showsInteger = shows

-- | Show a number as a fraction.
showsFraction :: Integer -> Integer -> Integer -> ShowS
showsFraction n e d = showsScientific n e . showChar '/' . showsInteger d

-- | Show a number in decimal or scientific notation.
showsScientific :: Integer -> Integer -> ShowS
showsScientific n e =
  addSign . if abs e <= 2 * len         -- e might be extremely large
            then shorter fixed floating
            else floating
  where nS       = showsInteger (abs n)
        len      = fromIntegral (length (nS ""))
        lenPred  = pred len
        fixed    = moveDot (-e) nS
        floating = moveDot lenPred nS . showsExponent (e + lenPred)
        addSign  = if signum n == -1 then ('-' :) else id

-- | Show the exponent (as part of the scientific notation).
showsExponent :: Integer -> ShowS
showsExponent 0 = id
showsExponent p = ('e' :) . showsInteger p

-- | Choose the shorter string, preferring the left string.
shorter :: ShowS -> ShowS -> ShowS
shorter s s' = if length (s' "") < length (s "") then s' else s

-- | Move the decimal point by the given amount (positive numbers for left).
moveDot :: Integer -> ShowS -> ShowS
moveDot i s = (++) $ reverse . stripDot . insertDot i . reverse $ s ""
  where stripDot ('.' : l) = l
        stripDot l         = l

-- | Insert a dot (@\'.\'@) before the given index, padding with zeros as
--   necessary.  Negative numbers are accepted.
insertDot :: Integer -> String -> String
insertDot i s = case compare i 0 of
  GT -> case s of
    []     -> '0' : insertDot (pred i) s
    x : xs ->  x  : insertDot (pred i) xs
  EQ -> '.' : s
  LT -> insertDot (succ i) ('0' : s)
