{-
Stability: experimental
-}
module Data.SciRatio.Utils
       ( parseNumber
       , prettyNumber
       , pNumber
       , isBinDigit
       , readBin
       , showBin
       ) where
import Control.Monad (ap, mzero)
import Data.Char (isAlpha, isDigit, toLower)
import Data.Ratio (denominator, numerator)
import Data.SciRatio (SciRatio, expPart, ratioPart)
import Numeric (readDec, readHex, readInt, readOct, showIntAtBase)
import Text.ParserCombinators.ReadP (ReadP, (<++))
import qualified Text.ParserCombinators.ReadP as P

-- | Whether the string begins with a prefix indicating its radix (e.g. @0x@).
hasRadixPrefix :: String -> Bool
hasRadixPrefix ('0' : c : _) = isAlpha c
hasRadixPrefix _             = False

-- | Convert a binary literal into a positive number.  The result is
--   undefined if the input is not a valid binary literal.
readBin :: Num a => ReadS a
readBin = readInt 2 isBinDigit digitToInt
  where digitToInt c = fromEnum c - fromEnum '0'

-- | Convert a positive number into its binary string representation.
showBin :: (Integral a, Show a) => a -> ShowS
showBin x = showIntAtBase 2 intToDigit x
  where intToDigit = toEnum . (+ fromEnum '0') . fromIntegral

-- | Whether the character is a binary digit (either @\'0\'@ or @\'1\'@).
isBinDigit :: Char -> Bool
isBinDigit c = fromEnum '0' <= x && x <= fromEnum '1' where x = fromEnum c

-- | Whether the character can be used as an exponent symbol (@\'e\'@,
--   @\'E\'@, or @\'\\x23e8\'@) in scientific notation.
isDecimalExponentSymbol :: Char -> Bool
isDecimalExponentSymbol = (`elem` "eE\x23e8")

-- | Whether the character is a fraction separator (@\'/\'@ or @\'\\x2044\'@).
isFractionSeparator :: Char -> Bool
isFractionSeparator = (`elem` "/\x2044")

-- | Parse a number prefixed by an optional sign.  The Unicode minus sign is
--   accepted.
pSigned :: Num a =>
           ReadP a                      -- ^ Parser for the number.
        -> ReadP a
pSigned p = do
  sign <- (<++ return 1) $ do
    c <- P.get
    case c of
      '+'      -> return   1
      '-'      -> return (-1)
      '\x2212' -> return (-1)
      _        -> mzero
  num <- p
  return (sign * num)

-- | Parse a natural number (unsigned integer) of arbitrary size in decimal
--   form.  Accepts a nonempty string of digits.
pNatural :: Num a => ReadP a
pNatural = fromInteger `fmap` P.readS_to_P readDec

-- | Parse an (signed) integer.
pInteger :: Num a => ReadP a
pInteger = pSigned pNatural

-- | Parse an unsigned decimal fraction.  Accepts a nonempty string of digits
--   with a single optional decimal point placed anywhere in the string.
pUnsignedDecimal :: Fractional a => ReadP a
pUnsignedDecimal = do
  intPart  <- P.munch isDigit
  string   <- P.look
  case string of
    '.' : _ -> do
      _ <- P.get
      fracPart <- P.munch isDigit
      if length intPart + length fracPart == 0
        then P.pfail
        else return $ readInt' intPart +
                      readInt' fracPart / 10 ^^ length fracPart
    _ ->
      case intPart of
        "" -> P.pfail
        _  -> return $ readInt' intPart
  where readInt' "" = 0
        readInt' s  = fromInteger (read s)

-- | Parse a (signed) decimal fraction.
pDecimal :: Fractional a => ReadP a
pDecimal = pSigned pUnsignedDecimal

-- | Parse an exponent in scientific notation.  Accepts a decimal exponent
--   symbol followed by an integer.
pSciExponent :: Num a => ReadP a
pSciExponent = P.satisfy isDecimalExponentSymbol >> pInteger

-- | Parse an unsigned scientific decimal fraction.
pUnsignedSciDecimal :: Fractional a => ReadP a
pUnsignedSciDecimal =
  fmap (*) pDecimal `ap` fmap pow10 (pSciExponent <++ return 0)
  where pow10 = (10 ^^) :: Fractional a => Integer -> a

-- | Parse a (signed) scientific decimal fraction.
pSciDecimal :: Fractional a => ReadP a
pSciDecimal = pSigned pUnsignedSciDecimal

-- | Parse a ratio of two numbers, each of which is parsed with the given
--   parser.
pRatio :: Fractional a =>
          ReadP a               -- ^ Parser for the numerator and denominator.
       -> ReadP a
pRatio p = fmap (/) p `ap` ((P.satisfy isFractionSeparator >> p) <++ return 1)

-- | Parse a natural number in either binary (@0b@), octal (@0o@), or
--   hexadecimal (@0x@) format (but not decimal).  The prefix is not
--   case-sensitive.
pNaturalBOH_ :: Num a => ReadP a
pNaturalBOH_ =
  P.char '0' >> fromInteger `fmap` P.choice
  [ char' 'x' >> P.readS_to_P readHex
  , char' 'o' >> P.readS_to_P readOct
  , char' 'b' >> P.readS_to_P readBin ]
  where char' x = P.satisfy $ \ c -> toLower c == toLower x

-- | Parse the most general number format: either @'naturalBOH_'@ or
--   @'pRatio pSciDecimal'@.
pNumber :: Fractional a => ReadP a
pNumber = do
  string <- P.look
  if hasRadixPrefix string
    then pNaturalBOH_
    else pRatio pSciDecimal

runReadPFull :: ReadP a -> String -> Maybe a
runReadPFull p s = case P.readP_to_S p s of
  [(x, [])] -> Just x
  _         -> Nothing

-- | Pretty-print the number in the format accepted by 'parseNumber'.
prettyNumber :: (Real a, Integral b) => SciRatio a b -> String
prettyNumber num = prettysNumber num ""

-- | Parse a generic, rational number.
parseNumber :: (Fractional a, Real a, Integral b) =>
               String -> Maybe (SciRatio a b)
parseNumber = runReadPFull pNumber

prettysNumber :: (Real a, Integral b) => SciRatio a b -> ShowS
prettysNumber num =
  let r = toRational (ratioPart num)
      e = toInteger (expPart num)
      n = numerator r
      d = denominator r in
  -- canonicity ensures that the divisor is not a multiple of 2 nor 5
  case d of
    1 -> prettysDecimal n e
    _ -> prettysFraction n e d

prettysExponent :: Integer -> ShowS
prettysExponent 0 = showString ""
prettysExponent p = showString "e" . showsInteger p

prettysFraction :: Integer -> Integer -> Integer -> ShowS
prettysFraction n e d = prettysDecimal n e . showString "/" . showsInteger d

showsInteger :: Integer -> ShowS
showsInteger = shows

-- | Choose the shorter string, preferring the left string.
shorter :: ShowS -> ShowS -> ShowS
shorter s s' = if length (s' "") < length (s "") then s' else s

moveDot :: Integer -> ShowS -> ShowS
moveDot i s = showString $ reverse . stripDot . insertDot i . reverse $ s ""
  where stripDot ('.' : l) = l
        stripDot l         = l

-- | Insert a decimal point at the given position relative to the
--   beginning, padding with zeros as necessary
insertDot :: Integer -> String -> String
insertDot i s = case compare i 0 of
  GT -> case s of
    []     -> '0' : insertDot (pred i) s
    x : xs ->  x  : insertDot (pred i) xs
  EQ -> '.' : s
  LT -> insertDot (succ i) ('0' : s)

-- | Render the decimal form of a number.
prettysDecimal :: Integer -> Integer -> ShowS
prettysDecimal n e =
  if abs e <= 2 * nsLen                 -- e might be extremely large
  then shorter fixed floating
  else floating
  where ns       = showsInteger (abs n)
        nsLen    = fromIntegral (length (ns ""))
        e'       = e + nsLen - 1
        fixed    = addSign . moveDot (-e) ns
        floating = addSign . moveDot (nsLen - 1) ns . prettysExponent e'
        addSign  = if signum n == -1 then ('-' :) else id
