\chapter{Rounding fixed point numbers}
\label{appendix:numbers}

\DeclarePairedDelimiter\ceil{\lceil}{\rceil}
\DeclarePairedDelimiter\floor{\lfloor}{\rfloor}

This somewhat arcane-looking code implements a wrapper around the base
library's decimal fixed point type, only using a more precise rounding
method for the |x*y| and |x/y| operations.

%if 0

> {-# Language GeneralizedNewtypeDeriving #-}
> {-# Language ScopedTypeVariables #-}

%endif

> module Maker.Decimal (Decimal, E18, E36, Epsilon (..)) where
>
> import Data.Fixed
>
> newtype HasResolution e => Decimal e = D (Fixed e)
>   deriving (Ord, Eq, Real, RealFrac)

We want the printed representations of these numbers to look like
|"0.01"| and not |"R 0.01"|.

> instance HasResolution e => Read (Decimal e) where
>   readsPrec n s = fmap (\(x, y) -> (D x, y)) (readsPrec n s)
> instance HasResolution e => Show (Decimal e) where
>   show (D x)  = show x

In the |Num| instance, we delegate everything except multiplication.

> instance HasResolution e => Num (Decimal e) where
>   x@(D (MkFixed a)) * D (MkFixed b) =
>     D (MkFixed (div  (a * b + div (resolution x) 2)
>                      (resolution x)))
> 
>   D a + D b      = D (a + b)
>   D a - D b      = D (a - b)
>   negate  (D a)  = D (negate a)
>   abs     (D a)  = D (abs a)
>   signum  (D a)  = D (signum a)
>   fromInteger i  = D (fromInteger i)

In the |Fractional| instance, we delegate everything except division.

> instance HasResolution e => Fractional (Decimal e) where
>   x@(D (MkFixed a)) / D (MkFixed b) =
>     D (MkFixed (div (a * resolution x + div b 2) b))
> 
>   recip (D a)     = D (recip a)
>   fromRational r  = D (fromRational r)

We define the |E18| and |E36| symbols and their fixed
point multipliers.

> data E18; data E36
>
> instance HasResolution E18 where
>   resolution _ = 10^(18 :: Integer)
> instance HasResolution E36 where
>   resolution _ = 10^(36 :: Integer)

The fixed point number types have well-defined smallest increments
(denoted |epsilon|).  This becomes useful when verifying equivalences.

> class Epsilon t where epsilon :: t
>
> instance HasResolution a => Epsilon (Decimal a) where
>   -- The use of |undefined| is safe since |resolution| ignores the value.
>   epsilon = 1 / fromIntegral (resolution (undefined :: Fixed a))

