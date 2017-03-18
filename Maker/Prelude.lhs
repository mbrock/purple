%if false

> {-# Language AllowAmbiguousTypes #-}
> {-# Language ConstraintKinds #-}
> {-# Language DuplicateRecordFields #-}
> {-# Language FlexibleContexts #-}
> {-# Language FlexibleInstances #-}
> {-# Language FunctionalDependencies #-}
> {-# Language GeneralizedNewtypeDeriving #-}
> {-# Language ImplicitParams #-}
> {-# Language LambdaCase #-}
> {-# Language MultiWayIf #-}
> {-# Language NoMonomorphismRestriction #-}
> {-# Language RankNTypes #-}
> {-# Language RecordWildCards #-}
> {-# Language ScopedTypeVariables #-}
> {-# Language StandaloneDeriving #-}
> {-# Language TemplateHaskell #-}
> {-# Language TypeFamilies #-}

%endif

\chapter{Prelude}
\label{appendix:prelude}

This module reexports symbols from other packages and exports a few
new symbols of its own.

> module Maker.Prelude (module Maker.Prelude, module X) where
>
> import Prelude as X (
>
> -- Conversions to and from strings
>   Read (..), Show (..),
>
> -- Comparisons
>   Eq (..), Ord (..),
>
> -- Core abstractions
>   Functor      (fmap),
>   Applicative  (),
>   Monad        (return, (>>=)),
>
> -- Numeric classes
>   Num (..), Integral (), Enum (),
>
> -- Numeric conversions
>   Real (..), Fractional (..),
>   RealFrac (..),
>   fromIntegral,
>
> -- Simple types
>   Integer, Int, String,
>
> -- Algebraic types
>   Bool    (True, False),
>   Maybe   (Just, Nothing),
>   Either  (Right, Left),
>
> -- Functional operators
>   (.), ($),
> -- Numeric operators
>   (+), (-), (*), (/), (^), (^^), div,
>
> -- Utilities
>   all, not, elem, (&&),
>
> -- Constants
>   mempty, undefined, otherwise)

We use a typical composition of monad transformers from the
\texttt{mtl} library to structure stateful actions.
See section~\ref{section:maker-monad} (\textit{The Maker monad}).

> import Control.Monad.State as X (
>   StateT,              -- Type constructor that adds state to a monad type
>   execStateT,          -- Runs a state monad with given initial state
>   get,                 -- Gets the state in a |do| block
>   put)                 -- Sets the state in a |do| block
>
> import Control.Monad.Writer as X (
>   WriterT,             -- Type constructor that adds logging to a monad type
>   Writer,              -- Type constructor of logging monads
>   runWriterT,          -- Runs a writer monad transformer
>   execWriterT,         -- Runs a writer monad transformer keeping only logs
>   execWriter)          -- Runs a writer monad keeping only logs
>
> import Control.Monad.Except as X (
>   MonadError,          -- Type class of monads that fail
>   Except,              -- Type constructor of failing monads
>   throwError,          -- Short-circuits the monadic computation
>   runExcept)           -- Runs a failing monad

Our numeric types use decimal fixed-point arithmetic.

> import Data.Fixed as X (
>   Fixed (..),           -- Type constructor for numbers of given precision
>   HasResolution (..))   -- Type class for specifying precisions

We rely on the \texttt{lens} library for accessing nested values.
There is no need to understand the theory behind lenses to understand
this program.  The notation |a . b . c| denotes a nested accessor much
like \texttt{a.b.c} in C-style languages; for more details, consult
lens documentation\footnote{Gabriel Gonzalez's 2013 article
\textsl{\href{
http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
}{Program imperatively using Haskell}} is a good introduction.}.

%if 0

> -- Hidden from document because the type signatures
> -- that use these names are also hidden.
> import Control.Lens as X (Traversal', Getting)

%endif

> import Control.Lens as X (
>
>   Lens', lens,
>
>   makeLenses,        -- Defines lenses for record fields
>   makeFields,        -- Defines lenses for record fields
>   set,               -- Writes a lens
>   use, preuse,       -- Reads a lens from a state value
>   view,              -- Reads a lens from a value
>   ix,                -- Lens for map retrieval and updating
>   at,                -- Lens for map insertion
>
> -- Operators for partial state updates in |do| blocks:
>   (.=),              -- Replace
>   (-=), (+=),        -- Update arithmetically
>   (%=),              -- Update according to function
>   (?=))              -- Insert into map
>
> import Control.Lens.Zoom as X (zoom)

Where the Solidity code uses \texttt{mapping}, we use Haskell's
regular tree-based map type\footnote{We assume the axiom that Keccak
hash collisions are impossible.}.

> import Data.Map as X (
>   Map,         -- Type constructor for mappings
>   empty,       -- Polymorphic empty mapping
>   singleton,   -- Creates a mapping with a single key--value pair
>   fromList)    -- Creates a mapping with several key--value pairs

%if 0

> import Data.Monoid as X (First, (<>))
> import Control.Monad as X (unless)
> import Control.Arrow as X (first)
>

%endif

Finally we define some of our own convenience functions.

> decrease    a x = a -=  x
> increase    a x = a +=  x
> initialize  a x = a %=  (\case Nothing -> Just x; y -> y)
> prepend     a x = a %=  (x :)
> 
> x `notElem` xs = not (elem x xs)
