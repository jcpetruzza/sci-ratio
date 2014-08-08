{-
Stability: experimental

-}
module Data.SciRatio
    ( SciRatio
    , SciRational
    , (.^)
    , ratioPart
    , expPart
    , largestDivisiblePower
    ) where
import Data.Ratio ((%), denominator, numerator)
import Data.Hashable (Hashable(hashWithSalt))
infixl 7 :^, .^
infixl 1 ~~

-- Implementation note: a SciRatio must *always* remain in canonical form, so
-- don't use :^ directly unless you're absolutely sure it will stay canonical
-- (use .^ instead if you're unsure).

-- | Represents a floating ratio: a combination of a rational significand with
--   a base-10 exponent.  The significand has type @a@ while the exponent has
--   type @b@.
data SciRatio a b = !a :^ !b deriving Eq

-- | A specialization of 'SciRatio'.
type SciRational = SciRatio Rational Integer

instance (Fractional a, Real a, Read a, Integral b, Read b) =>
         Read (SciRatio a b) where
  readsPrec p = readParen (p > prec) $ \ s -> do
    (x,    s')   <- readsPrec (prec + 1) s
    (".^", s'')  <- lex s'
    (y,    s''') <- readsPrec (prec + 1) s''
    return (x .^ y, s''')

instance (Show a, Show b) => Show (SciRatio a b) where
  showsPrec p (x :^ y) = showParen (p > prec) $
                         showsPrec prec x .
                         showString " .^ " .
                         showsPrec prec y

instance (Eq a, Fractional a, Ord a, Integral b) => Ord (SciRatio a b) where
  p `compare` q = x `compare` y where (x, y, _) = p ~~ q

instance (Hashable a, Hashable b) => Hashable (SciRatio a b) where
  hashWithSalt s (r :^ e) = hashWithSalt s (magic, r, e)
    where magic = 0xaaa80d6b :: Int

instance (Fractional a, Real a, Integral b) => Num (SciRatio a b) where
  p + q             =  (x + y) .^ c where (x, y, c) = p ~~ q
  p - q             =  (x - y) .^ c where (x, y, c) = p ~~ q
  x :^ a * (y :^ b) =    x * y .^ (a + b)
  abs      (y :^ b) =    abs y :^ b
  negate   (y :^ b) = negate y :^ b
  signum            = (:^ 0) . signum . ratioPart
  fromInteger       = (.^ 0) . fromInteger

instance (Fractional a, Real a, Integral b) => Fractional (SciRatio a b) where
  x :^ a / (y :^ b) =   x / y .^ (a - b)
  recip    (y :^ b) = recip y .^ (-b)
  fromRational      = (.^ 0) . fromRational

instance (Fractional a, Real a, Integral b) => Real (SciRatio a b) where
  toRational (y :^ b) = toRational y * 10 ^^ b

instance (Fractional a, Real a, Integral b) => RealFrac (SciRatio a b) where
  properFraction q = (\(n, p) -> (n, fromRational p))
                     . properFraction $ toRational q

instance (Fractional a, Real a, Integral b) => Enum (SciRatio a b) where
  succ               = fromRational . succ . toRational
  pred               = fromRational . pred . toRational
  toEnum             = fromRational . toEnum
  fromEnum           = fromEnum . toRational
  enumFrom           = fmap fromRational . enumFrom . toRational
  enumFromThen   x   = fmap fromRational
                       . enumFromThen (toRational x)
                       . toRational
  enumFromTo     x   = fmap fromRational
                       . enumFromTo (toRational x)
                       . toRational
  enumFromThenTo x y = fmap fromRational
                       . enumFromThenTo (toRational x) (toRational y)
                       . toRational

-- | Precedence of the (.^) operator.
prec :: Int
prec = 7

-- | Constructs a floating ratio such that
--   @significand .^ exponent == significand * 10 ^^ exponent@.
(.^) :: (Fractional a, Real a, Integral b) =>
        a                               -- ^ significand
     -> b                               -- ^ exponent
     -> SciRatio a b
x .^ y = canonicalize $ x :^ y

-- | Ratio part of the floating ratio.
ratioPart :: SciRatio a b -> a
ratioPart (x :^ _) = x

-- | Exponent part of the floating ratio.
expPart :: SciRatio a b -> b
expPart   (_ :^ x) = x

-- | Converts a floating ratio into a 'Fractional' number.
fromSciRatio :: (Real a, Integral b, Fractional c) => SciRatio a b -> c
fromSciRatio (x :^ y) = realToFrac x * 10 ^^ y

{-# RULES
    "realToFrac/fromSciRatio[SciRational]"
      forall (x :: SciRational) .
      realToFrac x = fromSciRatio x
  #-}

-- | Matches the exponents.
(~~) :: (Fractional a, Integral b) => SciRatio a b -> SciRatio a b -> (a, a, b)
x :^ a ~~ y :^ b = (x * 10 ^^ (a - c), y * 10 ^^ (b - c), c)
  where c = minAbs a b

minAbs :: (Num a, Ord a) => a -> a -> a
minAbs x y = if abs x <= abs y then x else y

-- | Extract the largest power of the given base that divides the input
--   integer.  Returns the significand and exponent, satisfying:
--   @input_integer = significand * base ^ exponent@
largestDivisiblePower :: (Integral a, Integral b) =>
                         a              -- ^ base
                      -> a              -- ^ input integer
                      -> (a, b)         -- ^ significand and exponent
largestDivisiblePower base = go 0 1 0
  where go  _    _   _ 0 = (0, 0)
        go  _    0   e i = (i, e)
        go maxe next e i =
          if i `mod` power == 0
          then go maxe'   next'  (e + next) (i `div` power)
          else go next'' next'' e i
          where power  = base ^ next
                maxe'  = if maxe == 0 then maxe     else maxe - next
                next'  = if maxe == 0 then next * 2 else maxe `div` 2
                next'' = next `div` 2

-- | Convert a 'SciRatio' into canonical form by factoring out as many powers
--   of the base as possible.
canonicalize :: (Fractional a, Real a, Integral b) =>
                SciRatio a b -> SciRatio a b
canonicalize (r :^ e) =
  let r' = toRational r in
  case ( largestDivisiblePower 10 (numerator r')
       , largestDivisiblePower 2 (denominator r') ) of
    ((n, ne), (d, e2)) -> case largestDivisiblePower 5 d of
      (d', e5) -> case compare e2 e5 of
        EQ -> fromRational (n % d') :^ (e + ne - e5)
        LT -> fromRational ((n * 2 ^ (e5 - e2)) % d') :^ (e + ne - e5)
        GT -> fromRational ((n * 5 ^ (e2 - e5)) % d') :^ (e + ne - e2)
