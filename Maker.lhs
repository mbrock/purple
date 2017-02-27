\documentclass[twoside,12pt]{book}

\usepackage[a4paper]{geometry}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{graphicx}
\usepackage[hidelinks]{hyperref}
\usepackage{xcolor}

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
%format -= = "\mathrel{-\!\!=}"
%format <$> = "\mathbin{<\!\!\$\!\!>}"
%format <*> = "\mathbin{<\!\!\!*\!\!\!>}"
%format ==> = "\implies"
%format ? = "\,?\!"
%format ?= = "\mathrel{?\!\!=}"


%format cdp = "\textsc{cdp}"
%format dai = "\textsc{dai}"
%format dai = "\textsc{dai}"
%format eth = "\textsc{eth}"
%format mkr = "\textsc{mkr}"

%format cow = "\textsc{cow}"
%format tau = "\textsc{tau}"
%format era = "\textsc{era}"
%format rho = "\textsc{rho}"
%format phi = "\textsc{phi}"
%format axe = "\textsc{axe}"
%format bag = "\textsc{bag}"
%format con = "\textsc{con}"
%format dai = "\textsc{dai}"
%format sdr = "\textsc{sdr}"
%format vat = "\textsc{vat}"
%format fix = "\textsc{fix}"
%format gem = "\textsc{gem}"
%format hat = "\textsc{hat}"
%format how = "\textsc{how}"
%format ilk = "\textsc{ilk}"
%format ilks = "\textsc{ilk}s"
%format jar = "\textsc{jar}"
%format jars = "\textsc{jar}s"
%format lad = "\textsc{lad}"
%format lag = "\textsc{lag}"
%format mat = "\textsc{mat}"
%format par = "\textsc{par}"
%format pie = "\textsc{pie}"
%format pro = "\textsc{pro}"
%format sin = "\textsc{sin}"
%format tag = "\textsc{tag}"
%format tax = "\textsc{tax}"
%format urn = "\textsc{urn}"
%format urns = "\textsc{urn}s"
%format way = "\textsc{way}"
%format zzz = "\textsc{zzz}"
%format wad = "\textsc{wad}"
%format ray = "\textsc{ray}"
%format nat = "\textsc{nat}"
%format vow = "\textsc{vow}"
%format cat = "\textsc{cat}"

%format wad0
%format wad_dai
%format wad_gem
%format wad_mkr
%format era0
%format tau0
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

%format Gem = "\textsc{Gem}"
%format Lad = "\textsc{Lad}"
%format Ilk = "\textsc{Ilk}"
%format Ray = "\textsc{Ray}"
%format Urn = "\textsc{Urn}"
%format Wad = "\textsc{Wad}"
%format Jar = "\textsc{Jar}"
%format Vat = "\textsc{Vat}"
%format Wad = "\textsc{Wad}"
%format Ray = "\textsc{Ray}"
%format Nat = "\textsc{Nat}"

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
%format poke = "\texttt{poke}"
%format Poke = "\texttt{Poke}"
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

The ``Dai credit system'' is the smart contract system used by the
\MakerDAO{} to control the price stability and deflation of the |dai|
stablecoin by automatic modification of market incentives (via
deflation adjustment), and to provide trustless credit services to
Ethereum blockchain users.

New dai enter the money supply when a dai borrower posts an excess
of collateral to a ``collateralized debt position'' (|cdp|) and takes
out a loan.  The debt and collateral amounts are recorded in the
|cdp|, and (as time passes) the stability fees incurred by the |cdp|
owner are also recorded.  The collateral itself is held in a token
vault controlled by the \MakerDAO{}.

Any Ethereum account can borrow dai without any requirements beyond
posting and maintaining adequate collateral.  There are no term limits
on dai loans and borrowers are free to open or close |cdp|s at any
time.  The collateral held in |cdp|s collectively backs the value of
the dai in a fully transparent manner that anyone can verify.

\section{Motivation}

The version of this system that will be deployed on the Ethereum
blockchain is written in Solidity, which is a workable smart contract
implementation language.  The reasons for maintaining this ``reference
implementation'' in Haskell are, roughly:

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

> module Maker where

We import types for the decimal fixed-point arithmetic which we use
for amounts and rates.

> import Data.Fixed

We rely on the \texttt{lens} library for defining and using accessors
which otherwise tend to become long-winded in Haskell.  Since our
program has several nested records, this makes the code much clearer.
There is no need to understand the theory behind lenses to understand
this program.  All the reader needs to know is that |a . b . c|
denotes a nested accessor much like \texttt{a.b.c} in C-style
languages.  The rest should be obvious from context.

> import Control.Lens

We use a typical stack of monad transformers from the |mtl| library to
structure state-modifying actions.  Again, the reader does not need
any abstract understanding of monads.  They make our code clear and
simple by enabling |do| blocks to express exceptions, state,
and logging.

> import Control.Monad.Except
>   (MonadError, Except, throwError, runExcept)
> import Control.Monad.Reader
>   (MonadReader (..))
> import Control.Monad.State
>   (MonadState, StateT, execStateT, get, put)
> import Control.Monad.Writer
>   (MonadWriter, WriterT, runWriterT)

Some less interesting imports are omitted from this document.

%if 0

> import Data.Monoid (First)
> import Control.Monad (unless)
> import Control.Arrow (first)
> import Data.Map (Map, empty, singleton)
> import Data.Sequence (Seq)
> import Prelude hiding (lookup, log, sin)
>
> import qualified Control.Monad.Writer as Writer
> import qualified Data.Sequence as Sequence

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

>  -- Phantom types encode precision at compile time.
> data E18; data E36
>
> -- Specify $10^{-18}$ as the precision of |E18|.
> instance HasResolution E18 where
>   resolution _ = 10^(18 :: Integer)
>
> -- Specify $10^{-36}$ as the precision of |E36|.
> instance HasResolution E36 where
>   resolution _ = 10^(36 :: Integer)
>
> -- Create the distinct |wad| type for currency quantities.
> newtype Wad = Wad (Fixed E18)
>   deriving (  Ord, Eq, Num, Real, Fractional)
>
> -- Create the distinct |ray| type for precise rate quantities.
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

In calculations where a |wad| is multiplied by a |ray|, for example in
the deflation mechanism, we have to downcast in a way that loses
precision.  Haskell does not cast automatically, so unless you see the
following |cast| function applied, you can assume that precision
is unchanged.

> cast :: (Real a, Fractional b) => a -> b
> cast =
> -- Convert via fractional $n/m$ form.
>   fromRational . toRational

We also define a type for non-negative integers.

> newtype Nat = Nat Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)


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

\section{Identifier type}

There are several types of identifiers used in the system, and we can
use Haskell's type system to distinguish them.

> -- The type parameter is only used to create distinct types.
> -- For example, |Id Foo| and |Id Bar| are incompatible.
>
> data Id a = Id String
>   deriving (Show, Eq, Ord)

It turns out that we will in several places use mappings from IDs to
the value type corresponding to that ID type, so we define an alias
for such mapping types.

> type IdMap a = Map (Id a) a

We also have three predefined entities:

> -- The |dai| token address
> id_dai = Id "Dai"
>
> -- The |cdp| engine address
> id_vat = Id "Vat"
>
> -- The account with ultimate authority
> id_god = Id "God"

%if 0

> instance Read (Id a) where
>   readsPrec n s = first Id <$> readsPrec n s

%endif

\section{Structures}

[XXX: describe structures]

> data Lad = Lad deriving (Eq, Show)

\subsection{|Gem| --- Collateral token model}

> data Gem =
>   Gem {
>     gemTotalSupply  :: !Wad,
>     gemBalanceOf    :: !(Map (Id Lad)          Wad),
>     gemAllowance    :: !(Map (Id Lad, Id Lad)  Wad)
>
>   } deriving (Eq, Read, Show)
>
> makeFields ''Gem

\subsection{|Jar| --- Collateral token}

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
>   } deriving (Eq, Show, Read)
>
> makeFields ''Jar

\subsection{|Ilk| --- |cdp| type}

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
>   -- ???
>     ilkCow  :: !Ray,
>
>   -- Stability fee accumulator
>     ilkBag  :: !(Map Nat Ray)
>
>   } deriving (Eq, Show)
>
> makeFields ''Ilk

\subsection{|Urn| --- collateralized debt position (|cdp|)}

> data Urn = Urn {
>
>   -- Address of biting cat
>     urnCat  :: !(Maybe (Id Lad)),
>
>   -- Address of liquidating vow
>     urnVow  :: !(Maybe (Id Lad)),
>
>   -- Issuer
>     urnLad  :: !(Id Lad),
>
>   -- |cdp| type
>     urnIlk  :: !(Id Ilk),
>
>   -- Outstanding dai debt
>     urnCon  :: !Wad,
>
>   -- Collateral amount
>     urnPro  :: !Wad,
>
>   -- Last poked
>     urnPhi  :: !Nat
>
>   } deriving (Eq, Show)
>
> makeFields ''Urn

\subsection{|Vat| --- Dai creditor}

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

\subsection{System model}

> data System =
>   System {
>     systemVat      :: Vat,
>     systemEra      :: !Nat,
>     systemLads     :: IdMap Lad,   -- System users
>     systemSender   :: Id Lad
>
>   } deriving (Eq, Show)
>
> makeFields ''System

\subsection{Default data}

> defaultIlk :: Id Jar -> Ilk
> defaultIlk id_jar = Ilk {
>   ilkJar  = id_jar,
>   ilkAxe  = Ray 1,
>   ilkMat  = Ray 1,
>   ilkTax  = Ray 1,
>   ilkHat  = Wad 0,
>   ilkLag  = Nat 0,
>   ilkBag  = empty,
>   ilkCow  = Ray 1,
>   ilkRho  = Nat 0
> }

> defaultUrn :: Id Ilk -> Id Lad -> Urn
> defaultUrn id_ilk id_lad = Urn {
>   urnVow  = Nothing,
>   urnCat  = Nothing,
>   urnLad  = id_lad,
>   urnIlk  = id_ilk,
>   urnCon  = Wad 0,
>   urnPro  = Wad 0,
>   urnPhi  = Nat 0
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
>   systemLads     = empty,
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
>   |  Give     (Id Urn)  (Id Lad)
>   |  Grab     (Id Urn)
>   |  Heal     Wad
>   |  Lock     (Id Urn)  Wad
>   |  Loot     Wad
>   |  Mark     (Id Jar)  Wad       Nat
>   |  Open     (Id Urn)  (Id Ilk)
>   |  Prod
>   |  Poke     (Id Urn)
>   |  Pull     (Id Jar)  (Id Lad)  Wad
>   |  Shut     (Id Urn)
>   |  Tell     Wad
>   |  Warp     Nat
>   |  Wipe     (Id Urn)  Wad
>   |  NewJar   (Id Jar)  Jar
>   |  NewLad   (Id Lad)
>   deriving (Eq, Show, Read)

Acts which are logged through the |note| modifier record the sender ID
and the act descriptor.

> data Log = LogNote (Id Lad) Act
>   deriving (Show, Eq)

Acts can fail.  We divide the failure modes into general assertion
failures and authentication failures.

> data Error = AssertError | AuthError
>   deriving (Show, Eq)

Now we can define the type of a

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
> sure :: Fails m => Bool -> m ()
> sure x = unless x (throwError AssertError)
>
> need :: (Fails m, Reads r m)
>      => Getting (First a) r a -> m a
> need f = preview f >>= \case
>   Nothing -> throwError AssertError
>   Just x  -> return x

\section{Modifiers}

\newcommand{\actentry}[2]
  {\addcontentsline{toc}{subsection}{#1 --- #2}}

\actentry{|note|}{logging actions}

> note ::
>   (  IsAct, Logs m,
>      Reads r m,
>        HasSender r (Id Lad))
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
>        HasSender r (Id Lad))
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

\actentry{|gaze|}{|urn|: identify |cdp| risk stage}

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
>   -- Market value of collateral
>     pro_sdr = view pro urn0 * view tag jar0
>   -- Debt at |dai| target price
>     con_sdr = view con urn0 * par0
>
>   in if
>   -- Undergoing liquidation?
>     | view vow  urn0  /= Nothing                -> Dread
>   -- Liquidation triggered?
>     | view cat  urn0  /= Nothing                -> Grief
>   -- Undercollateralized?
>     | pro_sdr < con_sdr * view mat ilk0         -> Panic
>   -- Price feed expired?
>     | era0 > view zzz jar0 + view lag ilk0      -> Panic
>   -- Price feed in limbo?
>     | view zzz  jar0  < era0                    -> Worry
>   -- Debt ceiling reached?
>     | view cow  ilk0  > view hat ilk0           -> Anger
>   -- Safely overcollateralized.
>     | otherwise                                 -> Pride

Now we define the internal act |gaze| which returns the value of
|analyze| after ensuring the system state is updated.

> gaze id_urn = do
>   prod
>   poke id_urn
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

\actentry{|open|}{|vat|: create |cdp| account}

> open id_urn id_ilk =
>   note $ do
>     id_lad <- view sender
>     vat . urns . at id_urn ?= defaultUrn id_ilk id_lad

\actentry{|lock|}{|urn|: deposit collateral}

> lock id_urn x =
>
>   note $ do
>
>   -- Ensure |cdp| exists; identify collateral type
>     id_ilk  <- need (urnAt  id_urn  . ilk)
>     id_jar  <- need (ilkAt  id_ilk  . jar)
>
>   -- Record an increase in collateral
>     urnAt id_urn . pro += x
>
>   -- Take sender's tokens
>     id_lad  <- view sender
>     pull id_jar id_lad x

\actentry{|free|}{|urn|: withdraw collateral}

> free id_urn wad_gem =
>
>   note $ do
>
>   -- Fail if sender is not the |cdp| owner.
>     id_sender  <- view sender
>     id_lad     <- need (urnAt id_urn . lad)
>     sure (id_sender == id_lad)
>
>   -- Tentatively record the decreased collateral.
>     urnAt id_urn . pro  -=  wad_gem
>
>   -- Fail if collateral decrease results in undercollateralization.
>     gaze id_urn >>= sure . (== Pride)
>
>   -- Send the collateral to the |cdp| owner.
>     id_ilk  <- need (urnAt  id_urn  . ilk)
>     id_jar  <- need (ilkAt  id_ilk  . jar)
>     push id_jar id_lad wad_gem

\actentry{|draw|}{|urn|: issue |dai| as debt}

> draw id_urn wad_dai =
>
>   note $ do
>
>   -- Fail if sender is not the |cdp| owner.
>     id_sender  <- view sender
>     id_lad     <- need (urnAt id_urn . lad)
>     sure (id_sender == id_lad)
>
>   -- Tentatively record |dai| debt.
>     urnAt id_urn . con += wad_dai
>
>   -- Fail if |cdp| with new debt is not overcollateralized.
>     gaze id_urn >>= sure . (== Pride)
>
>   -- Mint |dai| and send it to the |cdp| owner.
>     mint id_dai wad_dai
>     push id_dai id_lad wad_dai

\actentry{|wipe|}{|urn|: repay debt and burn |dai|}

> wipe id_urn wad_dai =
>
>   note $ do
>
>   -- Fail if sender is not the |cdp| owner.
>     id_sender  <- view sender
>     id_lad     <- need (urnAt id_urn . lad)
>     sure (id_sender == id_lad)
>
>   -- Fail if the |cdp| is not currently overcollateralized.
>     gaze id_urn >>= sure . (== Pride)
>
>   -- Preliminarily reduce the |cdp| debt.
>     urnAt id_urn . con -= wad_dai
>
>   -- Attempt to get back |dai| from |cdp| owner and destroy it.
>     pull id_dai id_lad wad_dai
>     burn id_dai wad_dai

\actentry{|give|}{|urn|: transfer |cdp| account}

> give id_urn id_lad =
>   note $ do
>     x <- need (urnAt id_urn . lad)
>     y <- view sender
>     sure (x == y)
>     urnAt id_urn . lad .= id_lad

\actentry{|shut|}{|urn|: wipe, free, and delete |cdp|}

> shut id_urn =
>
>   note $ do
>
>   -- Update the |cdp|'s debt (prorating the stability fee).
>     poke id_urn
>
>   -- Attempt to repay all the |cdp|'s outstanding |dai|.
>     con0 <- need (urnAt id_urn . con)
>     wipe id_urn con0
>
>   -- Reclaim all the collateral.
>     pro0 <- need (urnAt id_urn . pro)
>     free id_urn pro0
>
>   -- Nullify the |cdp|.
>     vat . urns . at id_urn .= Nothing

\clearpage
\section{Frequent adjustments}

\actentry{|prod|}{|vat|: perform revaluation and rate adjustment}

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

\actentry{|drip|}{|ilk|: update stability fee accumulator}

This internal act happens on every |poke|. It is also invoked when
governance changes the |tax| of an |ilk|.

> drip id_ilk = do
>
> -- Current time stamp
>   era0  <- view era
>
> -- Current stability fee
>   tax0  <- need (ilkAt id_ilk . tax)
>   cow0  <- need (ilkAt id_ilk . cow)
>
> -- Previous time and stability fee thus far
>   rho0  <- need (ilkAt id_ilk . rho)
>   ice   <- need (ilkAt id_ilk . bag . ix rho0)
>
>   let
>
>   -- Seconds passed
>     age   = era0 - rho0
>
>   -- Stability fee accrued since last drip
>     dew   = ice * tax0 ^^ age
>
>   -- I don't understand this calculation
>     cow1  = cow0 * (dew / ice)
>
>   ilkAt id_ilk . bag . at era0  ?= dew
>   ilkAt id_ilk . cow            .= cow1
>   ilkAt id_ilk . rho            .= era0
>
>   return dew

\actentry{|poke|}{|urn|: add stability fee to |cdp| debt}

> poke id_urn =
>
>   note $ do
>
>   -- Read previous stability fee accumulator.
>     id_ilk  <- need (urnAt id_urn  . ilk)
>     phi0    <- need (urnAt id_urn  . phi)
>     ice     <- need (ilkAt id_ilk  . bag . ix phi0)
>
>   -- Update the stability fee accumulator.
>     con0    <- need (urnAt id_urn  . con)
>     dew     <- drip id_ilk
>
>   -- Apply new stability fee to |cdp| debt.
>     urnAt id_urn . con *= cast (dew / ice)
>
>   -- Record the poke time.
>     era0 <- view era
>     urnAt id_urn . phi .= era0

\section{Governance}

\actentry{|form|}{|vat|: create a new |cdp| type}

> form id_ilk id_jar =
>   auth . note $ do
>     vat . ilks . at id_ilk ?= defaultIlk id_jar

\actentry{|frob|}{|vat|: set the sensitivity parameter}

> frob how' =
>   auth . note $ do
>     vat . how .= how'

\section{Price feedback}

\actentry{|mark|}{|vat|: update market price of |dai|}

> mark id_jar tag1 zzz1 =
>   auth . note $ do
>     jarAt id_jar . tag  .= tag1
>     jarAt id_jar . zzz  .= zzz1

\actentry{|tell|}{|gem|: update market price of collateral token}

> tell x =
>   auth . note $ do
>     vat . fix .= x

\section{Liquidation and settlement}

\actentry{|bite|}{|urn|: trigger |cdp| liquidation}

> bite id_urn =
>
>   note $ do
>
>   -- Fail if urn is not undercollateralized.
>     gaze id_urn >>= sure . (== Panic)
>
>   -- Record the sender as the liquidation initiator.
>     id_cat              <- view sender
>     urnAt id_urn . cat  .= id_cat
>
>   -- Read current debt.
>     con0    <- need (urnAt id_urn  . con)
>
>   -- Read liquidation penalty ratio.
>     id_ilk  <- need (urnAt id_urn  . ilk)
>     axe0    <- need (ilkAt id_ilk  . axe)
>
>   -- Apply liquidation penalty to debt.
>     let con1 = con0 * axe0
>
>   -- Update debt and record it as in need of settlement.
>     urnAt id_urn . con   .= con1
>     sin                  += con1

\actentry{|grab|}{|urn|: promise that liquidation is in process}

> grab id_urn =
>
>   auth . note $ do
>
>   -- Fail if |cdp| liquidation is not initiated.
>     gaze id_urn >>= sure . (== Grief)
>
>   -- Record the sender as the |cdp|'s settler.
>     id_vow <- view sender
>     urnAt id_urn . vow .= id_vow
>
>   -- Nullify the |cdp|'s debt and collateral.
>     pro0 <- need (urnAt id_urn .pro)
>     urnAt id_urn . con  .= 0
>     urnAt id_urn . pro  .= 0
>
>   -- Send the collateral to the settler for auctioning.
>     id_ilk <- need (urnAt id_urn  . ilk)
>     id_jar <- need (ilkAt id_ilk  . jar)
>     push id_jar id_vow pro0

\actentry{|heal|}{|vat|: process bad debt}

> heal wad_dai =
>
>   auth . note $ do
>
>     vat . sin -= wad_dai

\actentry{|loot|}{|vat|: process stability fee revenue}

> loot wad_dai =
>
>   auth . note $ do
>
>     vat . pie -= wad_dai

\section{Minting, burning, and transferring}

\actentry{|pull|}{|gem|: receive tokens to |vat|}

> pull id_jar id_lad w = do
>   g   <- need (jarAt id_jar . gem)
>   g'  <- transferFrom id_lad id_vat w g
>   jarAt id_jar . gem .= g'

\actentry{|push|}{|gem|: send tokens from |vat|}

> push id_jar id_lad w = do
>   g   <- need (jarAt id_jar . gem)
>   g'  <- transferFrom id_vat id_lad w g
>   jarAt id_jar . gem .= g'

\actentry{|mint|}{|dai|: increase supply}

> mint id_jar wad0 = do
>   jarAt id_jar . gem . totalSupply            += wad0
>   jarAt id_jar . gem . balanceOf . ix id_vat  += wad0

\actentry{|burn|}{|dai|: decrease supply}

> burn id_jar wad0 = do
>   jarAt id_jar . gem . totalSupply            -= wad0
>   jarAt id_jar . gem . balanceOf . ix id_vat  -= wad0
\section{Test-related manipulation}

\actentry{|warp|}{travel in time}

> warp t =
>   auth . note $ do
>     era += t

\section{System modelling}

> newLad id_lad = lads.at id_lad ?= Lad

> newLad ::
>   ( Writes w m, HasLads w (IdMap Lad))
>   => Id Lad -> m ()


> newJar id id_jar =
>   auth . note $ do
>     vat . jars .at id ?= id_jar

> newJar ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasJars vat_w (IdMap Jar))
>   =>
>     Id Jar -> Jar -> m ()

\section{Other stuff}

> perform :: Act -> Maker ()
> perform x =
>   let ?act = x in case x of
>     NewLad id        -> newLad id
>     NewJar id jar    -> newJar id jar
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
>   =>  Id Lad -> Id Lad -> Wad
>   ->  Gem -> m Gem
>
> transferFrom src dst wad gem =
>   case gem ^. balanceOf.(at src) of
>     Nothing ->
>       throwError AssertError
>     Just balance -> do
>       sure (balance >= wad)
>       return $ gem
>         & balanceOf . ix src -~ wad
>         & balanceOf . at dst %~
>             (\case
>                 Nothing  -> Just wad
>                 Just x   -> Just (wad + x))
>



\chapter{Testing}

\appendix

\chapter{Act type signatures}

We see that |drip| may fail; it reads an |ilk|'s |tax|, |cow|, |rho|,
and |bag|; and it writes those same parameters except |tax|.

> drip ::
>   (  Fails m,
>      Reads r m,
>        HasEra r Nat,
>        HasVat r vat_r,
>          HasIlks vat_r (Map (Id Ilk) ilk_r),
>            HasTax ilk_r Ray,
>            HasCow ilk_r Ray,
>            HasRho ilk_r Nat,
>            HasBag ilk_r (Map Nat Ray),
>      Writes w m,
>        HasVat w vat_w,
>          HasIlks vat_w (Map (Id Ilk) ilk_w),
>            HasCow ilk_w Ray,
>            HasRho ilk_w Nat,
>            HasBag ilk_w (Map Nat Ray))
>   => Id Ilk -> m Ray


> form ::
>
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasIlks vat_w (IdMap Ilk))
>
>  => Id Ilk -> Id Jar -> m ()

> frob :: (  IsAct, Fails m, Logs m,
>            Reads r m,   HasSender r (Id Lad),
>            Writes w m,  HasVat w vat_w,
>                           HasHow vat_w ray)
>   => ray -> m ()

> open ::
>   (  IsAct, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasUrns vat_w (IdMap Urn))
>   => Id Urn -> Id Ilk -> m ()

> give ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>                   HasVat r vat_r,
>                     HasUrns vat_r (Map (Id Urn) urn_r),
>                       HasLad urn_r (Id Lad),
>      Writes w m,  HasVat w vat_r)
>   => Id Urn -> Id Lad -> m ()

> lock ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,
>        HasSender r (Id Lad),
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
>            HasPro urn_w Wad)
>   => Id Urn -> Wad -> m ()

> mark ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasJars vat_w (Map (Id Jar) jar_w),
>                       HasTag jar_w wad,
>                       HasZzz jar_w nat)
>   => Id Jar -> wad -> nat -> m ()

> tell ::
>   (  IsAct, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasFix vat_w wad)
>   => wad -> m ()

> prod ::
>   (  IsAct, Logs m,
>      Reads r m,
>        HasSender r (Id Lad),
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
>      Reads r m,   HasSender r (Id Lad),
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
>   => Id Jar -> Id Lad -> Wad -> m ()

> push ::
>   (  Fails m,
>      Reads r m,
>        HasVat r vat_r,  HasJars vat_r (Map (Id Jar) jar_r),
>                           HasGem jar_r Gem,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r))
>   => Id Jar -> Id Lad -> Wad -> m ()

> mint ::
>   (  Fails m,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r),
>                           HasGem jar_r gem_r,
>                             HasTotalSupply  gem_r Wad,
>                             HasBalanceOf    gem_r (Map (Id Lad) Wad))
>   => Id Jar -> Wad -> m ()

> burn ::
>   (  Fails m,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r),
>                           HasGem jar_r gem_r,
>                             HasTotalSupply  gem_r Wad,
>                             HasBalanceOf    gem_r (Map (Id Lad) Wad))
>   => Id Jar -> Wad -> m ()



\end{document}
