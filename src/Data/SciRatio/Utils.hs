{-
Stability: experimental

-}
module Data.SciRatio.Utils where
import Control.Applicative ((<*>), (*>), (<$>), (<$))
import Data.Char (isAlpha, isDigit)
import Data.Ratio (denominator, numerator)
import Data.SciRatio (SciRatio, expPart, largestDivisiblePower, ratioPart)
import Numeric (readDec, readHex, readInt, readOct, showIntAtBase)
import Text.ParserCombinators.ReadP (ReadP, (<++))
import qualified Text.ParserCombinators.ReadP as P

-- | Convert a binary literal into a positive number.  The result is
--   undefined if the input is not a valid binary literal.
readBin :: Num a => ReadS a
readBin = readInt 2 isBinDigit digitToInt
  where digitToInt c = fromEnum c - fromEnum '0'

-- | Convert a positive number into its binary string representation.
showBin :: (Integral a, Show a) => a -> String
showBin x = showIntAtBase 2 intToDigit x ""
  where intToDigit = toEnum . (+ fromEnum '0') . fromIntegral

-- | Parse a binary digit (either @\'0\'@ or @\'1\'@).
isBinDigit :: Char -> Bool
isBinDigit c = fromEnum '0' <= x && x <= fromEnum '1' where x = fromEnum c

hasRadixPrefix :: String -> Bool
hasRadixPrefix ('0' : c : _) = isAlpha c
hasRadixPrefix _             = False

-- | A decimal point (@\'.\'@).
pDecimalPoint :: ReadP ()
pDecimalPoint = () <$ P.char '.'

-- | Parse an exponent symbol (@\'e\'@, @\'E\'@, or @\'\\x23e8\'@) in
--   scientific notation.
pDecimalExponentSymbol :: ReadP ()
pDecimalExponentSymbol = () <$ P.satisfy (`elem` "eE\x23e8")

-- | Parse a fraction separator (@\'/\'@ or @\'\\x2044\'@).
pFractionSeparator :: ReadP ()
pFractionSeparator = () <$ P.satisfy (`elem` "/\x2044")

-- | Parse a decimal digit (from @\'0\'@ to @\'9\'@).
pDigit :: ReadP Char
pDigit = P.satisfy isDigit

-- | Parse a number prefixed by an optional sign.  The Unicode minus sign is
--   accepted.
pSigned :: Num a =>
           ReadP a                      -- ^ Parser for the number.
        -> ReadP a
pSigned = ((*) <$> (sign <++ return 1) <*>)
  where sign = do
          c <- P.get
          case c of
            '+'      -> return   1
            '-'      -> return (-1)
            '\x2212' -> return (-1)
            _        -> P.pfail

-- | Parse a natural number (unsigned integer) of arbitrary size.  Accepts a
--   nonempty string of 'pDigit's.
pNatural :: Num a => ReadP a
pNatural = fromInteger <$> P.readS_to_P readDec

-- | Parse an integer (signed).  Accepts a 'pSigned' 'pNatural'.
pInteger :: Num a => ReadP a
pInteger = pSigned pNatural

-- | Parse an unsigned decimal fraction.  Accepts a nonempty string of
--   'pDigit's with a single optional 'pDecimalPoint' placed anywhere in the
--   string.
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

-- | Parse a (signed) decimal fraction.  Accepts a 'pSigned'
--   'pUnsignedDecimal'.
pDecimal :: Fractional a => ReadP a
pDecimal = pSigned pUnsignedDecimal

-- | Parse an exponent in scientific notation.  Accepts a
--   'pDecimalExponentSymbol' followed by an 'pInteger'.
pSciExponent :: Num a => ReadP a
pSciExponent = pDecimalExponentSymbol *> pInteger

-- | Parse an unsigned scientific decimal fraction.  Equivalent to
--   an 'pUnsignedDecimal' optionally followed by a 'pSciExponent'.
pUnsignedSciDecimal :: Fractional a => ReadP a
pUnsignedSciDecimal =
  (*) <$> pDecimal <*> (pow10 <$> pSciExponent <++ return 0)
  where pow10 = (10 ^^) :: Fractional a => Integer -> a

-- | Parse a (signed) scientific decimal fraction.  Accepts a 'pSigned'
--   'pUnsignedSciDecimal'.
pSciDecimal :: Fractional a => ReadP a
pSciDecimal = pSigned pUnsignedSciDecimal

-- | Parse a ratio of two numbers.
pRatio :: Fractional a =>
          ReadP a               -- ^ Parser for the numerator and denominator.
       -> ReadP a
pRatio p = (/) <$> p <*> (pFractionSeparator *> p) <++ return 1

-- | Parse a 'pRatio' of 'pInteger's.
pIntegerRatio :: Fractional a => ReadP a
pIntegerRatio = pRatio pInteger

-- | Parse a 'pRatio' of 'pDecimal's.
pDecimalRatio :: Fractional a => ReadP a
pDecimalRatio = pRatio pDecimal

-- | Parse a 'pRatio' of 'pSciDecimal's.
pSciDecimalRatio :: Fractional a => ReadP a
pSciDecimalRatio = pRatio pSciDecimal

-- | Parse a natural number in either binary (@0b@), octal (@0o@), or
--   hexadecimal (@0x@) format (but not decimal).  Uppercased prefixes are
--   allowed as well.
pNaturalBOH_ :: Num a => ReadP a
pNaturalBOH_ =
  fromInteger <$> P.choice
  [ P.string "0x" *> P.readS_to_P readHex
  , P.string "0o" *> P.readS_to_P readOct
  , P.string "0b" *> P.readS_to_P readBin ]

-- | Parse a natural number in either binary, octal, decimal, or hexadecimal
--   format.  See 'pNaturalBOH_' for details on accepted formats.
pNaturalBOH :: Num a => ReadP a
pNaturalBOH = do
  string <- P.look
  if hasRadixPrefix string
    then pNaturalBOH_
    else pNatural

-- | Parse the most general number format: either 'naturalBOH_' or
--   'pSciDecimalRatio'.
pNumber :: Fractional a => ReadP a
pNumber = do
  string <- P.look
  if hasRadixPrefix string
    then pNaturalBOH_
    else pSciDecimalRatio

-- | Parse the string using the 'pNumber' parser.
parseNumber :: (Fractional a, Real a, Integral b) =>
               String -> Maybe (SciRatio a b)
parseNumber s = case P.readP_to_S pNumber s of
  [(x, [])] -> Just x
  _         -> Nothing

-- | Pretty-print the number in the format accepted by 'parseNumber'.
prettyNumber :: (Real a, Integral b) => SciRatio a b -> String
prettyNumber num = case d of
  1 -> if abs e <= toInteger (length ns) -- e might be extremely large
       then shorter fixed floating
       else floating
       where ns = show n
             fixed = moveDot (-e) ns
             floating = ns ++ showExponent e
  _ -> case largestDivisiblePower 2 d of
    (1, f) -> decimalOrFraction (f, 5)
    _      -> case largestDivisiblePower 5 d of
      (1, f) -> decimalOrFraction (f, 2)
      _      -> fraction
  where r = toRational (ratioPart num)
        e = toInteger (expPart num)
        n = numerator r
        d = denominator r
        showExponent p = if p == 0 then "" else "e" ++ show p
        fraction       = show n ++ showExponent e ++ "/" ++ show d
        decimalOrFraction w = shorter (decimal w) fraction

        -- | Render the decimal form of a number.
        decimal (f, b) =
          if abs (f - e) <= nsLen       -- e might be extremely large
          then shorter fixed floating
          else floating
          where fixed    = addSign (moveDot (f - e) ns)
                floating = addSign (moveDot (nsLen - 1) ns ++ showExponent e')
                addSign = if signum n' == -1 then ('-' :) else id
                n' = n * b ^ f
                e' = e - f + nsLen - 1
                ns = show (abs n')
                nsLen = toInteger (length ns)

        -- | Choose the shorter string, preferring the left string.
        shorter s s' = if length s' < length s then s' else s
        moveDot i = reverse . stripDot . insertDot i . reverse
          where stripDot ('.' : l) = l
                stripDot l         = l

        -- | Insert a decimal point at the given position relative to the
        --   beginning, padding with zeros as necessary
        insertDot i l = case compare i 0 of
          GT -> case l of
            []     -> '0' : insertDot (pred i) l
            x : xs ->  x  : insertDot (pred i) xs
          EQ -> '.' : l
          LT -> insertDot (succ i) ('0' : l)
