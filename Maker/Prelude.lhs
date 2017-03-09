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

> module Maker.Prelude (
>   module Maker.Prelude,
>   module X
> ) where

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
>   all,
>
> -- Constants
>   mempty, undefined, otherwise)

We use a typical composition of monad transformers from the
\texttt{mtl} library to structure stateful actions.
See section~\ref{section:maker-monad} (\textit{The Maker monad}).

> import Control.Monad.State as X (
>   MonadState,          -- Type class of monads with state
>   StateT,              -- Type constructor that adds state to a monad type
>   execStateT,          -- Runs a state monad with given initial state
>   get,                 -- Gets the state in a |do| block
>   put)                 -- Sets the state in a |do| block
>
> import Control.Monad.Reader as X (
>   MonadReader,         -- Type class of monads with ``environments''
>   ask,                 -- Reads the environment in a |do| block
>   local)               -- Runs a sub-computation with a modified environment
>
> import Control.Monad.Writer as X (
>   MonadWriter,         -- Type class of monads that emit logs
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
>   Lens',
>   lens,
>
>   makeFields,        -- Defines lenses for record fields
>   set,               -- Writes a lens
>   use, preuse,
>   view, preview,     -- Reads a lens in a |do| block
>   (&~),              -- Lets us use a |do| block with setters \xxx{Get rid of this.}
>   ix,                -- Lens for map retrieval and updating
>   at,                -- Lens for map insertion
>
> -- Operators for partial state updates in |do| blocks:
>   (.=),              -- Replace
>   (-=), (+=),        -- Update arithmetically
>   (%=),              -- Update according to function
>   (?=))              -- Insert into map

Where the Solidity code uses \texttt{mapping}, we use Haskell's
regular tree-based map type\footnote{We assume the axiom that Keccak
hash collisions are impossible.}.

> import Data.Map as X (
>   Map,         -- Type constructor for mappings
>   empty,       -- Polymorphic empty mapping
>   singleton)   -- Creates a mapping with a single key--value pair

For sequences of log entries, we use a sequence structure which has better
time complexity than regular lists.

> import            Data.Sequence as X (Seq)
> import qualified  Data.Sequence as Sequence

Some less interesting imports are omitted from this document.

%if 0

> import Data.Monoid as X (First, (<>))
> import Control.Monad as X (unless)
> import Control.Arrow as X (first)
>
> import qualified Control.Monad.Writer as Writer

> write = Writer.tell . Sequence.singleton

%endif

