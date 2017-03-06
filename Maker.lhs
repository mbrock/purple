\documentclass[twoside,12pt]{book}

\usepackage[a4paper]{geometry}

\usepackage{amsmath}
\usepackage{scalefnt}
\usepackage{amssymb}
\usepackage{graphicx}
\usepackage[hidelinks]{hyperref}
\usepackage{xcolor}
\usepackage{parskip}

\hypersetup{colorlinks, linkcolor={blue!45!black}}

%include polycode.fmt
%include forall.fmt

%format family = "\mathbf{family}"

%subst comment a = "\mbox{\hspace{.1cm}\footnotesize{\textsf{" a "}}}"
%subst varid a = "\mathit{" a "}"
%subst conid a = "\mathrm{" a "}"

%format $ = "\mathrel{\,\$\,}"

%format empty = "\varnothing"
%format epsilon = "\epsilon"
%format @?= = "\cong"
%format ! = "\mathbin{ !}"
%format <$> = "\mathbin{<\!\!\$\!\!>}"
%format .= = "\mathrel{:=}"
%format += = "\mathrel{+\!\!=}"
%format *= = "\mathrel{*\!\!=}"
%format %= = "\mathrel{\%\!\!=}"
%format -= = "\mathrel{-\!\!=}"
%format <$> = "\mathbin{<\!\!\$\!\!>}"
%format <*> = "\mathbin{<\!\!\!*\!\!\!>}"
%format ==> = "\implies"
%format ? = "\,?\!"
%format ?= = "\mathrel{?\!\!=}"


%format cdp = "\textsc{cdp}"
%format dai = "\textsc{dai}"
%format eth = "\textsc{eth}"
%format mkr = "\textsc{mkr}"
%format sdr = "\textsc{sdr}"
%format xdr = "\textsc{xdr}"
%format imf = "\textsc{imf}"
%format erc20 = "\textsc{erc\scalefont{0.79}{20}}"

%format cow = "\texttt{cow}"
%format tau = "\texttt{tau}"
%format era = "\texttt{era}"
%format rho = "\texttt{rho}"
%format phi = "\texttt{phi}"
%format axe = "\texttt{axe}"
%format chi = "\texttt{chi}"
%format con = "\texttt{con}"
%format art = "\texttt{art}"
%format jam = "\texttt{jam}"
%format tab = "\texttt{tab}"
%format vat = "\texttt{vat}"
%format fix = "\texttt{fix}"
%format gem = "\texttt{gem}"
%format hat = "\texttt{hat}"
%format how = "\texttt{how}"
%format ilk = "\texttt{ilk}"
%format ilks = "\texttt{ilk}s"
%format jar = "\texttt{jar}"
%format jars = "\texttt{jar}s"
%format lad = "\texttt{lad}"
%format lag = "\texttt{lag}"
%format mat = "\texttt{mat}"
%format par = "\texttt{par}"
%format pie = "\texttt{pie}"
%format pro = "\texttt{pro}"
%format sin = "\texttt{sin}"
%format tag = "\texttt{tag}"
%format tax = "\texttt{tax}"
%format urn = "\texttt{urn}"
%format urns = "\texttt{urn}s"
%format way = "\texttt{way}"
%format zzz = "\texttt{zzz}"
%format wad = "\texttt{wad}"
%format ray = "\texttt{ray}"
%format nat = "\texttt{nat}"
%format vow = "\texttt{vow}"
%format cat = "\texttt{cat}"

%format wad0
%format wad_dai
%format wad_gem
%format wad_mkr
%format wad_chi
%format era0
%format tau0
%format chi0
%format chi1
%format fix0
%format par0
%format how0
%format way0
%format par1
%format how1
%format way1
%format tag1
%format zzz0
%format zzz1
%format tax0
%format cow0
%format cow1
%format rho0
%format pro0
%format con0
%format con1
%format axe0
%format vow0
%format cat0
%format tag0
%format lag0
%format hat0
%format mat0
%format urn0
%format ilk0
%format jar0

%format Gem = "\texttt{Gem}"
%format Lad = "\texttt{Lad}"
%format Ilk = "\texttt{Ilk}"
%format Ray = "\texttt{Ray}"
%format Urn = "\texttt{Urn}"
%format Wad = "\texttt{Wad}"
%format Jar = "\texttt{Jar}"
%format Vat = "\texttt{Vat}"
%format Wad = "\texttt{Wad}"
%format Ray = "\texttt{Ray}"
%format Nat = "\texttt{Nat}"

%format vat_r
%format vat_w
%format urn_r
%format urn_w
%format ilk_r
%format ilk_w
%format jar_r
%format jar_w

%format pro_sdr
%format con_sdr
%format min_sdr

%format Pride = "\texttt{Pride}"
%format Anger = "\texttt{Anger}"
%format Worry = "\texttt{Worry}"
%format Panic = "\texttt{Panic}"
%format Grief = "\texttt{Grief}"
%format Dread = "\texttt{Dread}"

%format pull = "\texttt{pull}"
%format wipe = "\texttt{wipe}"
%format plop = "\texttt{plop}"

%format mint = "\texttt{mint}"
%format Mint = "\texttt{Mint}"
%format burn = "\texttt{burn}"
%format Burn = "\texttt{Burn}"
%format bite = "\texttt{bite}"
%format Bite = "\texttt{Bite}"
%format pull = "\texttt{pull}"
%format Pull = "\texttt{Pull}"
%format push = "\texttt{push}"
%format Push = "\texttt{Push}"
%format wipe = "\texttt{wipe}"
%format Wipe = "\texttt{Wipe}"
%format draw = "\texttt{draw}"
%format Draw = "\texttt{Draw}"
%format form = "\texttt{form}"
%format Form = "\texttt{Form}"
%format free = "\texttt{free}"
%format Free = "\texttt{Free}"
%format frob = "\texttt{frob}"
%format Frob = "\texttt{Frob}"
%format give = "\texttt{give}"
%format Give = "\texttt{Give}"
%format grab = "\texttt{grab}"
%format Grab = "\texttt{Grab}"
%format heal = "\texttt{heal}"
%format Heal = "\texttt{Heal}"
%format lock = "\texttt{lock}"
%format Lock = "\texttt{Lock}"
%format loot = "\texttt{loot}"
%format Loot = "\texttt{Loot}"
%format mark = "\texttt{mark}"
%format Mark = "\texttt{Mark}"
%format tell = "\texttt{tell}"
%format Tell = "\texttt{Tell}"
%format open = "\texttt{open}"
%format Open = "\texttt{Open}"
%format prod = "\texttt{prod}"
%format Prod = "\texttt{Prod}"
%format gaze = "\texttt{gaze}"
%format Gaze = "\texttt{Gaze}"
%format drip = "\texttt{drip}"
%format Drip = "\texttt{Drip}"
%format shut = "\texttt{shut}"
%format Shut = "\texttt{Shut}"
%format swap = "\texttt{swap}"
%format Swap = "\texttt{Swap}"
%format note = "\texttt{note}"
%format Note = "\texttt{Note}"
%format auth = "\texttt{auth}"
%format Auth = "\texttt{Auth}"
%format warp = "\texttt{warp}"
%format Warp = "\texttt{Warp}"
%format aver = "\texttt{aver}"

%format id_urn
%format id_ilk
%format id_jar
%format id_lad
%format id_vow
%format id_cat
%format id_dai
%format id_vat
%format id_god
%format id_sender

\begin{document}

\begin{titlepage}
\centering

\vspace*{2cm}
\def\svgwidth{2cm}
\input{maker-logo.tex}
\par\vspace{1cm}
{\large \textsl{presents the}}
\par\vspace{0.3cm}
{\Large \scshape reference implementation}
\par \vspace{0.8cm}
{\large \textsl{of the remarkable}}
\par \vspace{0.4cm}
{\bfseries\LARGE DAI CREDIT SYSTEM} \par
\par \vspace{0.8cm}
{\large {issuing a diversely collateralized stablecoin}}


\vfill
{\textit{with last update on \today}.}

\end{titlepage}

\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{3}

\tableofcontents

\clearpage

\chapter{Introduction}

\newcommand{\MakerDAO}{\textsc{dai maker}}
\newcommand{\actentry}[2]
  {\addcontentsline{toc}{subsection}{#1 --- #2}}
\newcommand{\xxx}[1]{\textsl{\footnotesize $\Diamond$ #1}}

The \textsc{dai credit system}, henceforth also ``Maker,'' is a
network of Ethereum contracts designed to issue the |dai| currency
token and automatically adjust incentives in order to keep dai market
value stable relative to |sdr|\footnote{``Special Drawing Rights''
(ticker symbol |xdr|), the international reserve asset created by the
International Monetary Fund, whose value is derives from a weighted
basket of world currencies.  In the long term, the value of dai may
diverge from the value of |sdr|; whether in an inflationary or
deflationary way will depend on market forces.} in the short and
medium term.

New dai enters the money supply when a borrower takes out a loan
backed by an excess of collateral locked in Maker's token vault.
The debt and collateral amounts are recorded in a
\textit{collateralized debt position}, or |cdp|.  Thus all outstanding
dai represents some |cdp| owner's claim on their collateral.

Maker's knowledge of the market values of dai and the various tokens
used as collateral comes from \textit{price feeds}.  Prices are used
to continuously assess the risk of each |cdp|.  If the value of a
|cdp|'s collateral drops below a certain multiple of its debt, it is
marked for liquidation, which triggers a decentralized
auction mechanism.

Another token, |mkr|, is also controlled by Maker, acting as a
``share'' in the system itself.  When a |cdp| liquidation fails to
recover the full value of debt, Maker mints more |mkr| and auctions it
out.  Thus |mkr| is used to fund last resort market making.  The value
of the |mkr| token is based on the \textit{stability fee} imposed on
all dai loans: stability fee revenue goes toward buying |mkr|
for burning.

This document is an executable technical specification of the exact
workings of the Maker smart contracts.

\section{Reference implementation}

The version of this system that will be deployed on the Ethereum
blockchain is written in Solidity, which is a workable smart contract
implementation language.  This reference implementation is a precise
model of the behavior of those contracts, written as a ``literate''
Haskell program.  The motivations for such a reference implementation
include:

\begin{enumerate}

\item \textbf{Comparison.}  Checking two free-standing implementations
against each other is a well-known way of ensuring that they both
behave as intended.

\item \textbf{Testing.}  Haskell lets us use flexible and powerful
testing tools such as QuickCheck and SmallCheck for comprehensively
verifying key properties as a middle ground between unit testing and
formal verification.

\item \textbf{Explicitness.}  Coding the contract behavior in Haskell,
a purely functional language, enforces explicit description of aspects
which Solidity leaves implicit.  For example, a Solidity program can
read a previously unwritten mapping and get back a value initialized
with zeroed memory, whereas in Haskell we must explicitly describe
default values.  The state rollback behavior of failed actions is also
in Haskell explicitly coded as part of the monad transformer stack.

\item \textbf{Type correctness.}  While Solidity does have a static
type system, it is not expressive enough to encode the distinctions
made by our system.  In particular, the two different decimal fixed
point number types that we use are typed in Solidity with one and the
same \texttt{uint128} type.  In Haskell we can make this distinction
explicit.

\item \textbf{Formality.}  The work of translating a Solidity program
into a purely functional program opens up opportunities for certain
types of formal verification.  In particular, this document will be
useful for modelling aspects of the system in a proof assistant like
Agda, Idris, Coq, or Isabelle.  We can also use logical tools for
Haskell, such as Liquid Haskell (which provides compile time logical
property checking) and \texttt{sbv} (a toolkit for model checking and
symbolic execution).

\item \textbf{Simulation.}  Solidity is highly specific to the
Ethereum blockchain environment and as such does not have facilities
for interfacing with files or other computer programs.  This makes the
Solidity implementation of the system less useful for doing
simulations of the system's economic, game-theoretic, or
statistical aspects.

\end{enumerate}

\section{Prerequisite Haskell knowledge}

Some parts of this document require specific knowledge about Haskell
programming, but these parts only make up a framework for expressing
the more interesting parts in a natural way free of boilerplate.

\xxx{Guidelines for skipping boring chapters and so on...}

For a complete understanding of the reference implementation's source
code, the reader should grasp the following Haskell patterns:

\begin{itemize}

\item The use of |newtype| wrappers to distinguish different types of
values which have the same underlying type.

\item The use of |do| notation with the standard monad transformers:

\begin{itemize}
\item \texttt{StateT} for updating state,
\item \texttt{ReaderT} for the read-only environment,
\item \texttt{WriterT} for ``write-only state'' (namely logs), and
\item \texttt{ExceptT} for failures which roll back state changes.
\end{itemize}

\item The basic use of ``lenses'' (via the \texttt{lens} library) for
convenient reading and writing of specific parts of nested values.

\item The use of ``parametricity'' to express type-level guarantees
about how function parameters are used, especially for understanding
Appendix~\ref{appendix:types} which uses type signatures to specify
which parts of the system are used or altered by each system action.

\item \xxx{Some more stuff here...}

\end{itemize}

\part{Implementation}

\chapter{Preamble}

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

We declare the program's dependencies up front.  The reader should
probably skim this section and consult it later if unfamiliar with
some type or function.

> module Maker where

We use a typical composition of monad transformers from the
\texttt{mtl} library to structure stateful actions.  This becomes
relevant in section~\ref{section:maker-monad} (\textit{The Maker
monad}).

> import Control.Monad.State (
>   MonadState,          -- Type class of monads with state
>   StateT,              -- Type constructor that adds state to a monad type
>   execStateT,          -- Runs a state monad with given initial state
>   get,                 -- Gets the state in a |do| block
>   put)                 -- Sets the state in a |do| block
>
> import Control.Monad.Reader (
>   MonadReader,         -- Type class of monads with ``environments''
>   ask,                 -- Reads the environment in a |do| block
>   local)               -- Runs a sub-computation with a modified environment
>
> import Control.Monad.Writer (
>   MonadWriter,         -- Type class of monads that emit logs
>   WriterT,             -- Type constructor that adds logging to a monad type
>   runWriterT)          -- Runs a writer monad
> 
> import Control.Monad.Except (
>   MonadError,          -- Type class of monads that fail
>   Except,              -- Type constructor of failing monads
>   throwError,          -- Short-circuits the monadic computation
>   runExcept)           -- Runs a failing monad

Our numeric types use decimal fixed-point arithmetic.

> import Data.Fixed (
>   Fixed,                -- Type constructor for fixed-point numbers of given precision
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
> import Control.Lens (Traversal', Getting)

%endif

> import Control.Lens (
> 
>   makeFields,        -- Defines lenses for record fields
>   view, preview,     -- Reads a lens in a |do| block
>   (&~),              -- Lets us use a |do| block with setters \xxx{Get rid of this.}
>   ix,                -- Lens for map retrieval and updating
>   at,                -- Lens for map insertion
> 
> -- Operators for partial state updates in |do| blocks:
>   (.=),              -- Replace
>   (-=), (+=), (*=),  -- Update arithmetically
>   (%=),              -- Update according to function
>   (?=))              -- Insert into map

Where the Solidity code uses \texttt{mapping}, we use Haskell's
regular tree-based map type\footnote{We assume the axiom that Keccak
hash collisions are impossible.}.

> import Data.Map (
>   Map,         -- Type constructor for mappings
>   empty,       -- Polymorphic empty mapping
>   singleton)   -- Creates a mapping with a single key--value pair

For sequences of log entries, we use a sequence structure which has better
time complexity than regular lists.

> import            Data.Sequence (Seq)
> import qualified  Data.Sequence as Sequence

Some less interesting imports are omitted from this document.

%if 0

> import Data.Monoid (First)
> import Control.Monad (unless)
> import Control.Arrow (first)
> import Prelude hiding (lookup, log, sin)
>
> import qualified Control.Monad.Writer as Writer

%endif

\chapter{Types}

\section{Numeric types}

Many Ethereum tokens (e.g.~|eth|, |dai|, and |mkr|) are denominated with 18
decimals.  That makes decimal fixed point with 18 digits of precision
a natural choice for representing currency quantities.  We call such
quantities "wads" (as in "wad of cash").

For some quantities, such as the rate of deflation per second, we want
as much precision as possible, so we use twice the number of decimals.
We call such quantities "rays" (mnemonic "rate," but also imagine a
very precisely aimed ray of light).

>  -- Dummy types for specifying precisions
> data E18; data E36
>
> -- Specify $10^{-18}$ as the precision of |E18|
> instance HasResolution E18 where
>   resolution _ = 10^(18 :: Integer)
>
> -- Specify $10^{-36}$ as the precision of |E36|
> instance HasResolution E36 where
>   resolution _ = 10^(36 :: Integer)
>
> -- Create the distinct |wad| type for currency quantities
> newtype Wad = Wad (Fixed E18)
>   deriving (  Ord, Eq, Num, Real, Fractional)
>
> -- Create the distinct |ray| type for precise rate quantities
> newtype Ray = Ray (Fixed E36)
>   deriving (  Ord, Eq, Num, Real, Fractional)
>

%if 0

> instance Read Ray where
>   readsPrec n s = first Ray <$> readsPrec n s
> instance Read Wad where
>   readsPrec n s = first Wad <$> readsPrec n s
> instance Read Nat where
>   readsPrec n s = first Nat <$> readsPrec n s
>
> instance Show Wad  where show (Wad x)  = show x
> instance Show Ray  where show (Ray x)  = show x
> instance Show Nat  where show (Nat x)  = show x

%endif

In calculations that combine |wad|s and |ray|s, we have to convert
between the number types.  Haskell does not convert numbers
automatically, so when we explicitly need it, we use a
|cast| function.

> -- Convert via fractional $n/m$ form.
> cast :: (Real a, Fractional b) => a -> b
> cast = fromRational . toRational

We also define a type for non-negative integers.

> newtype Nat = Nat Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)

%if 0

\subsection{Epsilon values}

The fixed point number types have well-defined smallest increments
(denoted |epsilon|).  This becomes useful when verifying equivalences.

> class Epsilon t where epsilon :: t
>
> instance HasResolution a => Epsilon (Fixed a) where
>   -- The use of |undefined| is safe since |resolution| ignores the value.
>   epsilon = 1 / fromIntegral (resolution (undefined :: Fixed a))
>
> instance Epsilon Wad  where epsilon = Wad epsilon
> instance Epsilon Ray  where epsilon = Ray epsilon

%endif

\section{Identifier type}

There are several kinds of identifiers used in the system, and we can
use types to distinguish them.

> -- The type parameter |a| creates distinct types.
> -- For example, |Id Foo| and |Id Bar| are incompatible.
>
> data Id a = Id String
>   deriving (Show, Eq, Ord)

We will often use mappings from IDs to the value type corresponding to
that ID type, so we define an alias for such mappings.

> type IdMap a = Map (Id a) a

%if 0

> instance Read (Id a) where
>   readsPrec n s = first Id <$> readsPrec n s

%endif

\section{Domain types}

This section introduces the records stored by the Maker system.
The order of presentation is by use; types further down refer to types
further up, but not the other way around.

\actentry{|Address|}{Ethereum accounts}

> data Address = Address String
> 
>   deriving (Ord, Eq, Show)

We also have three predefined entities:

> -- The |dai| token address
> id_dai = Id "Dai"
>
> -- The |cdp| engine address
> id_vat = Address "Vat"
>
> -- The account with ultimate authority
> -- \xxx{Kludge until authority is modelled}
> id_god = Address "God"

\actentry{|Gem|}{|erc20| token model}

> data Gem =
>   Gem {
>     gemTotalSupply  :: !Wad,
>     gemBalanceOf    :: !(Map Address          Wad),
>     gemAllowance    :: !(Map (Address, Address)  Wad)
>
>   } deriving (Eq, Show)
>
> makeFields ''Gem

\actentry{|Jar|}{collateral type}

> data Jar = Jar {
>
>   -- Collateral token
>     jarGem  :: !Gem,
>
>   -- Market price
>     jarTag  :: !Wad,
>
>   -- Price expiration
>     jarZzz  :: !Nat
>
>   } deriving (Eq, Show)
>
> makeFields ''Jar

\actentry{|Ilk|}{|cdp| type}

> data Ilk = Ilk {
>
>   -- Collateral vault
>     ilkJar  :: !(Id Jar),
>
>   -- Liquidation penalty
>     ilkAxe  :: !Ray,
>
>   -- Debt ceiling
>     ilkHat  :: !Wad,
>
>   -- Liquidation ratio
>     ilkMat  :: !Ray,
>
>   -- Stability fee
>     ilkTax  :: !Ray,
>
>   -- Limbo duration
>     ilkLag  :: !Nat,
>
>   -- Last dripped
>     ilkRho  :: !Nat,
>
>   -- Total debt in dai
>     ilkDin  :: !Wad,
>
>   -- Price of debt coin
>     ilkChi  :: !Ray
>
>   } deriving (Eq, Show)
>
> makeFields ''Ilk

\actentry{|Urn|}{collateralized debt position (|cdp|)}

> data Urn = Urn {
>
>   -- Address of biting cat
>     urnCat  :: !(Maybe Address),
>
>   -- Address of liquidating vow
>     urnVow  :: !(Maybe Address),
>
>   -- Issuer
>     urnLad  :: !Address,
>
>   -- |cdp| type
>     urnIlk  :: !(Id Ilk),
>
>   -- Outstanding debt in debt coins
>     urnArt  :: !Wad,
>
>   -- Collateral amount in debt coins
>     urnJam  :: !Wad
>
>   } deriving (Eq, Show)
>
> makeFields ''Urn

\actentry{|Vat|}{|cdp| engine}

> data Vat = Vat {
>
>   -- Market price
>     vatFix  :: !Wad,
>
>   -- Sensitivity
>     vatHow  :: !Ray,
>
>   -- Target price
>     vatPar  :: !Wad,
>
>   -- Target rate
>     vatWay  :: !Ray,
>
>   -- Last prodded
>     vatTau  :: !Nat,
>
>   -- Unprocessed revenue from stability fees
>     vatPie  :: !Wad,
>
>   -- Bad debt from liquidated |cdp|s
>     vatSin  :: !Wad,
>
>   -- Collateral tokens
>     vatJars  :: !(IdMap Jar),
>
>   -- |cdp| types
>     vatIlks  :: !(IdMap Ilk),
>
>   -- |cdp|s
>     vatUrns  :: !(IdMap Urn)
>
>   } deriving (Eq, Show)
>
> makeFields ''Vat

\addcontentsline{toc}{subsection}{System model}

> data System =
>   System {
>     systemVat      :: Vat,
>     systemEra      :: !Nat,
>     systemSender   :: Address
>
>   } deriving (Eq, Show)
>
> makeFields ''System

\section{Default data}

> defaultIlk :: Id Jar -> Ilk
> defaultIlk id_jar = Ilk {
>   ilkJar  = id_jar,
>   ilkAxe  = Ray 1,
>   ilkMat  = Ray 1,
>   ilkTax  = Ray 1,
>   ilkHat  = Wad 0,
>   ilkLag  = Nat 0,
>   ilkChi  = Ray 1,
>   ilkDin  = Wad 0,
>   ilkRho  = Nat 0
> }

> defaultUrn :: Id Ilk -> Address -> Urn
> defaultUrn id_ilk id_lad = Urn {
>   urnVow  = Nothing,
>   urnCat  = Nothing,
>   urnLad  = id_lad,
>   urnIlk  = id_ilk,
>   urnArt  = Wad 0,
>   urnJam  = Wad 0
> }

> initialVat :: Ray -> Vat
> initialVat how0 = Vat {
>   vatTau   = 0,
>   vatFix   = Wad 1,
>   vatPar   = Wad 1,
>   vatHow   = how0,
>   vatWay   = Ray 1,
>   vatPie   = Wad 0,
>   vatSin   = Wad 0,
>   vatIlks  = empty,
>   vatUrns  = empty,
>   vatJars  =
>     singleton id_dai Jar {
>       jarGem   = Gem {
>         gemTotalSupply  = 0,
>         gemBalanceOf    = empty,
>         gemAllowance    = empty
>       },
>       jarTag  = Wad 0,
>       jarZzz  = 0
>     }
> }

> initialSystem :: Ray -> System
> initialSystem how0 = System {
>   systemVat      = initialVat how0,
>   systemEra      = 0,
>   systemSender   = id_god
> }

\chapter{Act framework}

\section{Act descriptions}

We define the Maker act vocabulary as a data type.  This is used for
logging and generally for representing acts.

> data Act =
>      Bite     (Id Urn)
>   |  Draw     (Id Urn)  Wad
>   |  Form     (Id Ilk)  (Id Jar)
>   |  Free     (Id Urn)  Wad
>   |  Frob     Ray
>   |  Give     (Id Urn)  Address
>   |  Grab     (Id Urn)
>   |  Heal     Wad
>   |  Lock     (Id Urn)  Wad
>   |  Loot     Wad
>   |  Mark     (Id Jar)  Wad       Nat
>   |  Open     (Id Urn)  (Id Ilk)
>   |  Prod
>   |  Poke     (Id Urn)
>   |  Pull     (Id Jar)  Address   Wad
>   |  Shut     (Id Urn)
>   |  Tell     Wad
>   |  Warp     Nat
>   |  Wipe     (Id Urn)  Wad
>   deriving (Eq, Show)

Acts which are logged through the |note| modifier record the sender ID
and the act descriptor.

> data Log = LogNote Address Act
>   deriving (Show, Eq)

Acts can fail.  We divide the failure modes into general assertion
failures and authentication failures.

> data Error = AssertError | AuthError
>   deriving (Show, Eq)

\section{The |Maker| monad}
\label{section:maker-monad}

The reader does not need any abstract understanding of monads to
understand the code.  What they give us is a nice syntax---the |do|
notation---for expressing exceptions, state, and logging in a way that
is still purely functional.

> newtype Maker a =
>   Maker (StateT System
>           (WriterT (Seq Log)
>             (Except Error)) a)
>
>   deriving (
>     Functor, Applicative, Monad,
>     MonadError   Error,
>     MonadState   System,
>     MonadWriter  (Seq Log)
>   )

> exec  ::  System
>       ->  Maker ()
>       ->  Either Error (System, Seq Log)
> exec sys (Maker m) =
>   runExcept (runWriterT (execStateT m sys))

> instance MonadReader System Maker where
>   ask = Maker get
>   local f (Maker m) = Maker $ do
>     s <- get;  put (f s)
>     x <- m;    put s
>     return x

\section{Constraints}

> type Reads   r  m = MonadReader r m
> type Writes  w  m = MonadState w m
> type Logs       m = MonadWriter (Seq Log) m
> type Fails      m = MonadError Error m
>
> type IsAct    = ?act :: Act
> type Notes      m = (IsAct, Logs m)

\section{Accessor aliases}

> ilkAt  id = vat . ilks  . ix id
> urnAt  id = vat . urns  . ix id
> jarAt  id = vat . jars  . ix id

%if 0

> ilkAt ::
>   ( HasIlks vat (Map (Id Ilk) ilk)
>   , HasVat s vat
>   ) => Id Ilk -> Traversal' s ilk
>
> urnAt ::
>   ( HasUrns vat (Map (Id Urn) urn)
>   , HasVat s vat
>   ) => Id Urn -> Traversal' s urn
>
> jarAt ::
>   ( HasJars vat (Map (Id Jar) jar)
>   , HasVat s vat
>   ) => Id Jar -> Traversal' s jar

%endif

\section{Logging and asserting}

> log :: Logs m => Log -> m ()
> log x = Writer.tell (Sequence.singleton x)
>
> aver :: Fails m => Bool -> m ()
> aver x = unless x (throwError AssertError)
>
> need :: (Fails m, Reads r m)
>      => Getting (First a) r a -> m a
> need f = preview f >>= \case
>   Nothing -> throwError AssertError
>   Just x  -> return x

\section{Modifiers}

\actentry{|note|}{logging actions}

> note ::
>   (  IsAct, Logs m,
>      Reads r m,
>        HasSender r Address)
>   => m a -> m a

> note k = do
>   s <- view sender
>   x <- k
>   log (LogNote s ?act)
>   return x

\actentry{|auth|}{authenticating actions}

> auth ::
>   (  IsAct, Fails m,
>      Reads r m,
>        HasSender r Address)
>   => m a -> m a

> auth continue = do
>   s <- view sender
>   unless (s == id_god)
>     (throwError AuthError)
>   continue

\newpage
\chapter{Acts}

We call the basic operations of the Dai credit system "acts."

\newpage
\section{Risk assessment}

\actentry{|gaze|}{identify |cdp| risk stage}

\newcommand{\yep}{$\bullet$}

\begin{table}[t]
\caption{Urn acts in the five stages of risk}\label{table:stages}
\vspace{0.25cm}
\resizebox{\textwidth}{!}{%
\begin{tabular}{ r c c c c c c c c c l }
&|give|&|shut|&|lock|&|wipe|&|free|&|draw|&|bite|&|grab|&|plop|& \\
\hline
|Pride|&\yep&\yep&\yep&\yep&\yep&\yep&&&& overcollateralized \\
\hline
|Anger|&\yep&\yep&\yep&\yep&\yep&&&&& debt ceiling reached \\
\hline
|Worry|&\yep&\yep&\yep&\yep&&&&&& price feed in limbo \\
\hline
|Panic|&\yep&\yep&\yep&\yep&&&\yep&&& undercollateralized \\
\hline
|Grief|&\yep&&&&&&&\yep&& liquidation initiated \\
\hline
|Dread|&\yep&&&&&&&&\yep& liquidation in progress \\
\hline
\end{tabular}}
\end{table}

We divide an urn's situation into five stages of risk.
Table \ref{table:stages} shows which acts each stage allows.
The stages are naturally ordered from more to less risky.

> data Stage  =  Dread | Grief | Panic | Worry | Anger | Pride
>
>   deriving (Eq, Ord, Show)

First we define a pure function |analyze| that determines an
urn's stage.

> analyze era0 par0 urn0 ilk0 jar0 =
>   let
>     cap  = view din ilk0  * cast (view chi ilk0)
>     pro  = view jam urn0  * view tag jar0
>     con  = view art urn0  * cast (view chi ilk0) * par0
>     min  = con * view mat ilk0
> 
>   in if
>   -- Undergoing liquidation?
>     | view vow  urn0  /= Nothing                -> Dread
>   -- Liquidation triggered?
>     | view cat  urn0  /= Nothing                -> Grief
>   -- Undercollateralized?
>     | pro < min                                 -> Panic
>   -- Price feed expired?
>     | era0 > view zzz jar0 + view lag ilk0      -> Panic
>   -- Price feed in limbo?
>     | view zzz  jar0  < era0                    -> Worry
>   -- Debt ceiling reached?
>     | cap  > view hat ilk0                      -> Anger
>   -- Safely overcollateralized.
>     | otherwise                                 -> Pride

Now we define the internal act |gaze| which returns the value of
|analyze| after ensuring the system state is updated.

> gaze id_urn = do
>   prod
> 
>   id_ilk  <- need (urnAt id_urn . ilk)
>   drip id_ilk
> 
>   era0    <- view era
>   par0    <- view (vat . par)
>
>   urn0    <- need (urnAt  id_urn)
>   ilk0    <- need (ilkAt  (view ilk urn0  ))
>   jar0    <- need (jarAt  (view jar ilk0  ))
> 
>   return (analyze era0 par0 urn0 ilk0 jar0)

\section{Lending}

\actentry{|open|}{create |cdp| account}

> open id_urn id_ilk =
>   note $ do
>     id_lad <- view sender
>     vat . urns . at id_urn ?= defaultUrn id_ilk id_lad

\actentry{|lock|}{deposit collateral}

> lock id_urn x =
>
>   note $ do
>
>   -- Ensure |cdp| exists; identify collateral type
>     id_ilk  <- need (urnAt  id_urn  . ilk)
>     id_jar  <- need (ilkAt  id_ilk  . jar)
>
>   -- Record an increase in collateral
>     urnAt id_urn . jam += x
>
>   -- Take sender's tokens
>     id_lad  <- view sender
>     pull id_jar id_lad x

\actentry{|free|}{withdraw collateral}

> free id_urn wad_gem =
>
>   note $ do
>
>   -- Fail if sender is not the |cdp| owner.
>     id_sender  <- view sender
>     id_lad     <- need (urnAt id_urn . lad)
>     aver (id_sender == id_lad)
>
>   -- Tentatively record the decreased collateral.
>     urnAt id_urn . jam  -=  wad_gem
>
>   -- Fail if collateral decrease results in undercollateralization.
>     gaze id_urn >>= aver . (== Pride)
>
>   -- Send the collateral to the |cdp| owner.
>     id_ilk  <- need (urnAt  id_urn  . ilk)
>     id_jar  <- need (ilkAt  id_ilk  . jar)
>     push id_jar id_lad wad_gem

\actentry{|draw|}{issue dai as debt}

> draw id_urn wad_dai =
>
>   note $ do
>
>   -- Fail if sender is not the |cdp| owner
>     id_sender  <- view sender
>     id_lad     <- need (urnAt id_urn . lad)
>     aver (id_sender == id_lad)
>
>   -- Update price of debt coin
>     id_ilk     <- need (urnAt id_urn . ilk)
>     chi1       <- drip id_ilk
>
>   -- Denominate draw amount in debt coin
>     let  wad_chi = wad_dai / cast chi1
>
>   -- Increase debt
>     urnAt id_urn . art += wad_chi
>
>   -- Roll back unless overcollateralized
>     gaze id_urn >>= aver . (== Pride)
>
>   -- Mint dai and send to the |cdp| owner
>     mint id_dai wad_dai
>     push id_dai id_lad wad_dai

\actentry{|wipe|}{repay debt and burn dai}

> wipe id_urn wad_dai =
>
>   note $ do
>
>   -- Fail if sender is not the |cdp| owner
>     id_sender  <- view sender
>     id_lad     <- need (urnAt id_urn . lad)
>     aver (id_sender == id_lad)
>
>   -- Update price of debt coin
>     id_ilk <- need (urnAt id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Denominate dai amount in debt coin
>     let  wad_chi = wad_dai / cast chi1
> 
>   -- Roll back if the |cdp| is not overcollateralized
>     gaze id_urn >>= aver . (== Pride)
>
>   -- Reduce debt
>     urnAt id_urn . art -= wad_chi
>
>   -- Take dai from |cdp| owner, or roll back
>     pull id_dai id_lad wad_dai
>
>   -- Destroy dai
>     burn id_dai wad_dai

\actentry{|give|}{transfer |cdp| account}

> give id_urn id_lad =
>   note $ do
>     x <- need (urnAt id_urn . lad)
>     y <- view sender
>     aver (x == y)
>     urnAt id_urn . lad .= id_lad

\actentry{|shut|}{wipe, free, and delete |cdp|}

> shut id_urn =
>
>   note $ do
>
>   -- Update price of debt coin
>     id_ilk <- need (urnAt id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Attempt to repay all the |cdp|'s outstanding dai
>     art0 <- need (urnAt id_urn . art)
>     wipe id_urn (art0 * cast chi1)
>
>   -- Reclaim all the collateral
>     jam0 <- need (urnAt id_urn . jam)
>     free id_urn jam0
>
>   -- Nullify the |cdp|
>     vat . urns . at id_urn .= Nothing

\clearpage
\section{Frequent adjustments}

\actentry{|prod|}{perform revaluation and rate adjustment}

> prod = note $ do
>
>   era0  <- view era
>   tau0  <- view (vat . tau)
>   fix0  <- view (vat . fix)
>   par0  <- view (vat . par)
>   how0  <- view (vat . how)
>   way0  <- view (vat . way)
>
>   let
>
>   -- Time difference in seconds
>     fan  = era0 - tau0
>
>   -- Current deflation rate applied to target price
>     par1  = par0 * cast (way0 ^^ fan)
>
>   -- Sensitivity parameter applied over time
>     wag  = how0 * fromIntegral fan
>
>   -- Deflation rate scaled up or down
>     way1  = inj (  prj way0 +
>                    if fix0 < par0 then wag else -wag)
>
>   vat.par  .= par1
>   vat.way  .= way1
>   vat.tau  .= era0
>
>   where
>
>   -- Convert between multiplicative and additive form
>     prj x  = if x >= 1  then x - 1  else 1 - 1 / x
>     inj x  = if x >= 0  then x + 1  else 1 / (1 - x)

\actentry{|drip|}{update price of debt coin}

This internal act happens on every |poke|. It is also invoked when
governance changes the |tax| of an |ilk|.

> drip id_ilk = do
>
> -- Current time stamp
>   era0  <- view era
>   rho0  <- need (ilkAt id_ilk . rho)
>
> -- Current stability fee
>   tax0  <- need (ilkAt id_ilk . tax)
>
> -- Current price of debt coin
>   chi0  <- need (ilkAt id_ilk . chi)
>
>   let
>
>     age   = era0 - rho0
>
>     chi1  = chi0 * tax0 ^^ age
>
>   ilkAt id_ilk . chi  .= chi1
>   ilkAt id_ilk . rho  .= era0
>
>   return chi1

\section{Governance}

\actentry{|form|}{create a new |cdp| type}

> form id_ilk id_jar =
>   auth . note $ do
>     vat . ilks . at id_ilk ?= defaultIlk id_jar

\actentry{|frob|}{set the sensitivity parameter}

> frob how' =
>   auth . note $ do
>     vat . how .= how'

\section{Price feedback}

\actentry{|mark|}{update market price of dai}

> mark id_jar tag1 zzz1 =
>   auth . note $ do
>     jarAt id_jar . tag  .= tag1
>     jarAt id_jar . zzz  .= zzz1

\actentry{|tell|}{update market price of collateral token}

> tell x =
>   auth . note $ do
>     vat . fix .= x

\section{Liquidation and settlement}

\actentry{|bite|}{mark |cdp| for liquidation}

> bite id_urn =
>
>   note $ do
>
>   -- Fail if urn is not undercollateralized
>     gaze id_urn >>= aver . (== Panic)
>
>   -- Record the sender as the requester of liquidation
>     id_cat              <- view sender
>     urnAt id_urn . cat  .= id_cat
>
>   -- Read current debt
>     art0    <- need (urnAt id_urn  . art)
> 
>   -- Update price of debt coin
>     id_ilk     <- need (urnAt id_urn . ilk)
>     chi1       <- drip id_ilk
>
>   -- Read liquidation penalty ratio
>     id_ilk  <- need (urnAt id_urn  . ilk)
>     axe0    <- need (ilkAt id_ilk  . axe)
>
>   -- Apply liquidation penalty to debt
>     let art1 = art0 * axe0
>
>   -- Update debt and record it as in need of settlement
>     urnAt id_urn . art   .= art1
>     sin                  += art1 * chi1

\actentry{|grab|}{take tokens to begin |cdp| liquidation}

> grab id_urn =
>
>   auth . note $ do
>
>   -- Fail if |cdp| is not marked for liquidation
>     gaze id_urn >>= aver . (== Grief)
>
>   -- Record the sender as the |cdp|'s settler
>     id_vow <- view sender
>     urnAt id_urn . vow .= id_vow
>
>   -- Clear the |cdp|'s requester of liquidation
>     urnAt id_urn . cat .= Nothing

\actentry{|heal|}{process bad debt}

> heal wad_dai =
>
>   auth . note $ do
>
>     vat . sin -= wad_dai

\actentry{|loot|}{process stability fee revenue}

> loot wad_dai =
>
>   auth . note $ do
>
>     vat . pie -= wad_dai

\section{Minting, burning, and transferring}

\actentry{|pull|}{take tokens to vat}

> pull id_jar id_lad w = do
>   g   <- need (jarAt id_jar . gem)
>   g'  <- transferFrom id_lad id_vat w g
>   jarAt id_jar . gem .= g'

\actentry{|push|}{send tokens from vat}

> push id_jar id_lad w = do
>   g   <- need (jarAt id_jar . gem)
>   g'  <- transferFrom id_vat id_lad w g
>   jarAt id_jar . gem .= g'

\actentry{|mint|}{increase supply}

> mint id_jar wad0 = do
>   jarAt id_jar . gem . totalSupply            += wad0
>   jarAt id_jar . gem . balanceOf . ix id_vat  += wad0

\actentry{|burn|}{decrease supply}

> burn id_jar wad0 = do
>   jarAt id_jar . gem . totalSupply            -= wad0
>   jarAt id_jar . gem . balanceOf . ix id_vat  -= wad0

\section{Test-related manipulation}

\actentry{|warp|}{travel in time}

> warp t =
>   auth . note $ do
>     era += t

\section{Other stuff}

> perform :: Act -> Maker ()
> perform x =
>   let ?act = x in case x of
>     Form id jar      -> form id jar
>     Mark jar tag zzz -> mark jar tag zzz
>     Open id ilk      -> open id ilk
>     Tell wad         -> tell wad
>     Frob ray         -> frob ray
>     Prod             -> prod
>     Warp t           -> warp t
>     Give urn lad     -> give urn lad
>     Pull jar lad wad -> pull jar lad wad
>     Lock urn wad     -> lock urn wad
>
>
> transferFrom
>   ::  (MonadError Error m)
>   =>  Address -> Address -> Wad
>   ->  Gem -> m Gem
>
> transferFrom src dst wad gem =
>   case view (balanceOf . at src) gem of
>     Nothing ->
>       throwError AssertError
>     Just balance -> do
>       aver (balance >= wad)
>       return $ gem &~ do
>         balanceOf . ix src -= wad
>         balanceOf . at dst %=
>           (\case
>               Nothing  -> Just wad
>               Just x   -> Just (wad + x))
>



\chapter{Testing}

\appendix

\chapter{Act type signatures}
\label{appendix:types}

> type Numbers wad ray nat =
>   (wad ~ Wad, ray ~ Ray, nat ~ Nat)

We see that |drip| may fail; it reads an |ilk|'s |tax|, |cow|, |rho|,
and |bag|; and it writes those same parameters except |tax|.

> drip ::
>   (  Fails m,
>      Reads r m,
>        HasEra r Nat,
>        HasVat r vat_r,
>          HasIlks vat_r (Map (Id Ilk) ilk_r),
>            HasTax ilk_r Ray,
>            HasRho ilk_r Nat,
>            HasChi ilk_r Ray,
>      Writes w m,
>        HasVat w vat_w,
>          HasIlks vat_w (Map (Id Ilk) ilk_w),
>            HasRho ilk_w Nat,
>            HasChi ilk_w Ray)
>   => Id Ilk -> m Ray


> form ::
>
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasIlks vat_w (IdMap Ilk))
>
>  => Id Ilk -> Id Jar -> m ()

> frob :: (  IsAct, Fails m, Logs m,
>            Reads r m,   HasSender r Address,
>            Writes w m,  HasVat w vat_w,
>                           HasHow vat_w ray)
>   => ray -> m ()

> open ::
>   (  IsAct, Logs m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasUrns vat_w (IdMap Urn))
>   => Id Urn -> Id Ilk -> m ()

> give ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r Address,
>                   HasVat r vat_r,
>                     HasUrns vat_r (Map (Id Urn) urn_r),
>                       HasLad urn_r Address,
>      Writes w m,  HasVat w vat_r)
>   => Id Urn -> Address -> m ()

> lock ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,
>        HasSender r Address,
>        HasVat r vat_r,
>          HasUrns vat_r (Map (Id Urn) urn_r),
>            HasIlk urn_r (Id Ilk),
>          HasIlks vat_r (Map (Id Ilk) ilk_r),
>            HasJar ilk_r (Id Jar),
>          HasJars vat_r (Map (Id Jar) jar_r),
>            HasGem jar_r Gem,
>      Writes w m,
>        HasVat w vat_w,
>          HasJars vat_w (Map (Id Jar) jar_r),
>          HasUrns vat_w (Map (Id Urn) urn_w),
>            HasJam urn_w Wad)
>   => Id Urn -> Wad -> m ()

> mark ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasJars vat_w (Map (Id Jar) jar_w),
>                       HasTag jar_w wad,
>                       HasZzz jar_w nat)
>   => Id Jar -> wad -> nat -> m ()

> tell ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasFix vat_w wad)
>   => wad -> m ()

> prod ::
>   (  IsAct, Logs m,
>      Reads r m,
>        HasSender r Address,
>        HasEra r nat,
>        HasVat r vat_r,  (  HasPar vat_r wad,
>                            HasTau vat_r nat,
>                            HasHow vat_r ray,
>                            HasWay vat_r ray,
>                            HasFix vat_r wad),
>      Writes w m,
>        HasVat w vat_w,  (  HasPar vat_w wad,
>                            HasWay vat_w ray,
>                            HasTau vat_w nat),
>      Integral nat,
>      Ord wad, Fractional wad,
>      Fractional ray, Real ray)
>   => m ()

> warp ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasEra w nat,
>                     Num nat)
>   => nat -> m ()


> pull ::
>   (  Fails m,
>      Reads r m,
>        HasVat r vat_r,  HasJars vat_r (Map (Id Jar) jar_r),
>                           HasGem jar_r Gem,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r))
>   => Id Jar -> Address -> Wad -> m ()

> push ::
>   (  Fails m,
>      Reads r m,
>        HasVat r vat_r,  HasJars vat_r (Map (Id Jar) jar_r),
>                           HasGem jar_r Gem,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r))
>   => Id Jar -> Address -> Wad -> m ()

> mint ::
>   (  Fails m,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r),
>                           HasGem jar_r gem_r,
>                             HasTotalSupply  gem_r Wad,
>                             HasBalanceOf    gem_r (Map Address Wad))
>   => Id Jar -> Wad -> m ()

> burn ::
>   (  Fails m,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r),
>                           HasGem jar_r gem_r,
>                             HasTotalSupply  gem_r Wad,
>                             HasBalanceOf    gem_r (Map Address Wad))
>   => Id Jar -> Wad -> m ()

> grab ::
>   (  IsAct, Fails m, Logs m,
>      Numbers wad ray nat,
>      Reads r m,
>        HasSender r Address,
>        HasEra r Nat,
>        HasVat r vat_r,
>          HasFix vat_r wad,
>          HasPar vat_r wad,
>          HasHow vat_r ray,
>          HasWay vat_r ray,
>          HasTau vat_r nat,
>          HasUrns vat_r (Map (Id Urn) urn_r),
>            HasJam urn_r wad,
>            HasArt urn_r wad,
>            HasCat urn_r (Maybe Address),  HasVow urn_r (Maybe Address),
>            HasIlk urn_r (Id Ilk),
>          HasIlks vat_r (Map (Id Ilk) ilk_r),
>            HasHat ilk_r wad,
>            HasMat ilk_r wad,
>            HasDin ilk_r wad,
>            HasTax ilk_r ray,
>            HasLag ilk_r nat,
>            HasChi ilk_r ray,  HasRho ilk_r nat,
>            HasJar ilk_r (Id Jar),
>          HasJars vat_r (Map (Id Jar) jar_r),
>            HasGem jar_r Gem,
>            HasTag jar_r wad,
>            HasZzz jar_r nat,
>      Writes w m,
>        HasVat w vat_w,
>          HasTau vat_w nat,
>          HasWay vat_w ray,  HasPar vat_w wad,
>          HasUrns vat_w (Map (Id Urn) urn_w),
>            HasJam urn_w wad,  HasArt urn_w wad,
>            HasVow urn_w Address,
>            HasCat urn_w (Maybe Address),
>          HasIlks vat_w (Map (Id Ilk) ilk_w),
>            HasChi ilk_w ray,
>            HasRho ilk_w nat,
>          HasJars vat_w (Map (Id Jar) jar_r)
>   ) => Id Urn -> m ()
>



\end{document}
