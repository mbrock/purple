\documentclass[twoside]{book}
\usepackage[a5paper]{geometry}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{xcolor}
\usepackage{graphicx}
%include polycode.fmt
%include forall.fmt

%format family = "\mathbf{family}"

%subst comment a = "\mbox{\hspace{.2cm} ${\Diamond}$\textsf{" a "}}"
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

%format wad0
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
%format zzz1
%format tax0
%format cow0
%format cow1
%format rho0

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

%format Pride = "\texttt{Pride}"
%format Anger = "\texttt{Anger}"
%format Worry = "\texttt{Worry}"
%format Panic = "\texttt{Panic}"
%format Grief = "\texttt{Grief}"
%format Dread = "\texttt{Dread}"

%format pull = "\texttt{pull}"
%format wipe = "\texttt{wipe}"
%format plop = "\texttt{plop}"

%format bite = "\texttt{bite}"
%format Bite = "\texttt{Bite}"
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

\section{The Dai credit system}

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

\section{This document}

\part{Implementation}

\chapter{Preamble}

%if false

> {-# Language AllowAmbiguousTypes #-}
> {-# Language TypeFamilies #-}
> {-# Language ConstraintKinds #-}
> {-# Language DuplicateRecordFields #-}
> {-# Language FlexibleContexts #-}
> {-# Language FlexibleInstances #-}
> {-# Language FunctionalDependencies #-}
> {-# Language GeneralizedNewtypeDeriving #-}
> {-# Language ImplicitParams #-}
> {-# Language LambdaCase #-}
> {-# Language NoMonomorphismRestriction #-}
> {-# Language RankNTypes #-}
> {-# Language RecordWildCards #-}
> {-# Language ScopedTypeVariables #-}
> {-# Language StandaloneDeriving #-}
> {-# Language TemplateHaskell #-}

%endif

> module Maker where

We import types for the decimal fixed-point arithmetic which we use
for amounts and rates.

> import Data.Fixed

We rely on the \texttt{lens} library for defining and using accessors
which otherwise tend to become long-winded in Haskell.  Since our
program has several nested records, this makes the code much clearer.

There is no need to understand the theory behind lenses to understand
this program.  All you need to know is that |a . b . c| denotes a
nested accessor much like \texttt{a.b.c} in C-style languages.
The rest should be obvious from context.

> import Control.Lens

We use a typical stack of monad transformers from the |mtl| library to
structure state-modifying actions.

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
> import Prelude hiding (lookup, log)
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

> newtype Wad = Wad (Fixed E18)
>   deriving (  Ord, Eq, Num, Real, Fractional)
 
> data E18 = E18
> instance HasResolution E18 where
>   resolution _ = 10^(18 :: Integer)

For some quantities, such as the rate of deflation per second, we want
as much precision as possible, so we use twice the number of decimals.
We call such quantities "rays" (mnemonic "rate," but also imagine a
very precisely aimed ray of light).

> newtype Ray = Ray (Fixed E36)
>   deriving (  Ord, Eq, Num, Real, Fractional)
 
> data E36 = E36
> instance HasResolution E36 where
>   resolution _ = 10^(36 :: Integer)

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
> cast = fromRational . toRational

We also define a type for non-negative integers.

> newtype Nat = Nat Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)


\subsection{Epsilon values}

The fixed point number types have well-defined smallest increments
(denoted |epsilon|).  This becomes useful when verifying equivalences.

> class Epsilon t where epsilon :: t
> 
> instance HasResolution a => Epsilon (Fixed a) where
>   epsilon = 1 / fromIntegral (resolution (undefined :: Fixed a))
> 
> instance Epsilon Wad  where epsilon = Wad epsilon
> instance Epsilon Ray  where epsilon = Ray epsilon

\section{Identifier type}

> data Id a = Id String
>   deriving (Show, Eq, Ord)
 
> instance Read (Id a) where
>   readsPrec n s = first Id <$> readsPrec n s

> type IdMap a = Map (Id a) a

\section{Structures}

> data Lad = Lad deriving (Eq, Show)

\subsection{|Gem| --- ERC20 token model}

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

> data Jar =
>   Jar {
>     jarGem  :: !Gem,          -- ERC20 token
>     jarTag  :: !Wad,          -- Market price
>     jarZzz  :: !Nat           -- Price expiration
> 
>   } deriving (Eq, Show, Read)
> 
> makeFields ''Jar

\subsection{|Ilk| --- CDP type}

> data Ilk =
>   Ilk {
>     ilkJar  :: !(Id Jar),       -- Collateral vault
>     ilkAxe  :: !Ray,            -- Liquidation penalty
>     ilkHat  :: !Wad,            -- Debt ceiling
>     ilkMat  :: !Ray,            -- Liquidation ratio
>     ilkTax  :: !Ray,            -- Stability fee
>     ilkLag  :: !Nat,            -- Limbo duration
>     ilkRho  :: !Nat,            -- Last dripped
>     ilkCow  :: !Ray,            -- ???
>     ilkBag  :: !(Map Nat Ray)   -- Stability fee accumulator
> 
>   } deriving (Eq, Show)
> 
> makeFields ''Ilk

\subsection{|Urn| --- CDP}

> data Urn =
>   Urn {
>     urnCat  :: !(Maybe (Id Lad)),  -- XXX
>     urnLad  :: !(Id Lad),          -- Issuer
>     urnIlk  :: !(Id Ilk),          -- CDP type
>     urnCon  :: !Wad,               -- Outstanding dai debt
>     urnPro  :: !Wad,               -- Collateral amount
>     urnPhi  :: !Nat                -- Last poked
>
>   } deriving (Eq, Show)
> 
> makeFields ''Urn

\subsection{|Vat| --- Dai creditor}

> data Vat =
>   Vat {
>     vatFix  :: !Wad,           -- Market price
>     vatHow  :: !Ray,           -- Sensitivity
>     vatPar  :: !Wad,           -- Target price
>     vatWay  :: !Ray,           -- Target rate
>     vatTau  :: !Nat,           -- Last prodded
>     vatPie  :: !Wad,           -- Unprocessed fees
>     vatSin  :: !Wad,           -- Bad debt
>     vatJars  :: !(IdMap Jar),  -- ERC20 tokens
>     vatIlks  :: !(IdMap Ilk),  -- CDP types
>     vatUrns  :: !(IdMap Urn)   -- CDPs
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
>     singleton (Id "DAI") Jar {
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
>   systemSender   = Id "God"
> }

\chapter{Action framework}

\section{Actions}

> data Action =
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
>   |  NewJar   (Id Jar)  Jar
>   |  NewLad   (Id Lad)
>   |  Open     (Id Urn)  (Id Ilk)
>   |  Prod
>   |  Poke     (Id Urn)
>   |  Pull     (Id Jar)  (Id Lad)  Wad
>   |  Shut     (Id Urn)
>   |  Tell     Wad
>   |  Warp     Nat
>   |  Wipe     (Id Urn)  Wad
>   deriving (Eq, Show, Read)

> data Log = LogNote (Id Lad) Action
>   deriving (Show, Eq)
> 
> data Error = AssertError | AuthError
>   deriving (Show, Eq)
> 
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
> 
> exec :: System -> Maker () -> Either Error (System, Seq Log)
> exec sys (Maker m) = runExcept (runWriterT (execStateT m sys))
> 
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
> type HasAction    = ?action :: Action
> type Notes      m = (HasAction, Logs m)

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

\subsection{\texttt{note} --- logging actions}

> note ::
>   (  HasAction, Logs m,
>      Reads r m,
>        HasSender r (Id Lad))
>   => m a -> m a

> note k = do
>   s <- view sender
>   x <- k
>   log (LogNote s ?action)
>   return x

\subsection{\texttt{auth} --- authenticating actions}
 
> auth ::
>   (  HasAction, Fails m,
>      Reads r m,
>        HasSender r (Id Lad))
>   => m a -> m a
 
> auth continue = do
>   s <- view sender
>   unless (s == Id "God")
>     (throwError AuthError)
>   continue

\newpage
\chapter{Acts}

\newcommand{\yep}{$\bullet$}

\begin{table}[h]
\resizebox{\textwidth}{!}{%
\begin{tabular}{ r c c c c c c c c c l }
        & |give| & |shut| & |lock| & |wipe| & |free| & |draw| & |bite| & |grab| & |plop| & \\
\hline
|Pride| & \yep & \yep & \yep & \yep & \yep & \yep & & & & overcollateralized \\
\hline
|Anger| & \yep & \yep & \yep & \yep & \yep &      & & & & debt ceiling reached \\
\hline
|Worry| & \yep & \yep & \yep & \yep &      &      & & & & stale price feed \\
\hline
|Panic| & \yep & \yep & \yep & \yep &      &      & \yep & & & undercollateralized \\
\hline
|Grief| & \yep & & & & & & & \yep & & liquidation initiated \\
\hline
|Dread| & \yep & & & & & & & & \yep & liquidation in progress \\
\hline
\end{tabular}}
\caption{Possible acts in the stages of an urn}
\end{table}

\newpage
\section{Acts performed by governance}

\subsection{|form| --- create a new |ilk|}

> form id_ilk id_jar =
>   auth . note $ do
>     vat . ilks . at id_ilk ?= defaultIlk id_jar
 
> form ::
>   (  HasAction, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                   HasIlks vat_w (IdMap Ilk))
>   =>
>     Id Ilk -> Id Jar -> m ()

\subsection{|frob| --- alter the sensitivity parameter}

> frob how' =
>   auth . note $ do
>     vat . how .= how'

> frob :: (  HasAction, Fails m, Logs m,
>            Reads r m,   HasSender r (Id Lad),
>            Writes w m,  HasVat w vat_w,
>                           HasHow vat_w ray)
>   => ray -> m ()
 
\section{Acts performed by account holders}

\subsection{|open| --- open CDP}

> open id_urn id_ilk =
>   note $ do
>     id_lad <- view sender
>     vat . urns . at id_urn ?= defaultUrn id_ilk id_lad

> open ::
>   (  HasAction, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasUrns vat_w (IdMap Urn))
>   => Id Urn -> Id Ilk -> m ()

\subsection{|give| --- transfer CDP}

> give id_urn id_lad =
>   note $ do
>     x <- need (urnAt id_urn . lad)
>     y <- view sender
>     sure (x == y)
>     urnAt id_urn . lad .= id_lad

> give ::
>   (  HasAction, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>                   HasVat r vat_r,
>                     HasUrns vat_r (Map (Id Urn) urn_r),
>                       HasLad urn_r (Id Lad),
>      Writes w m,  HasVat w vat_r)
>   => Id Urn -> Id Lad -> m ()
 
\subsection{|lock| --- insert collateral}

> lock id_urn x =
> 
>   note $ do
> 
>   -- Ensure CDP exists; identify collateral type
>     id_ilk  <- need (urnAt  id_urn  . ilk)
>     id_jar  <- need (ilkAt  id_ilk  . jar)
>
>   -- Record an increase in collateral
>     urnAt id_urn . pro += x
>
>   -- Take sender's tokens
>     id_lad  <- view sender
>     pull id_jar id_lad x

> lock ::
>   (  HasAction, Fails m, Logs m,
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

\section{Acts performed by price feeds}

\subsection{|mark| --- update DAI market price}

> mark id_jar tag1 zzz1 =
>   auth . note $ do
>     jarAt id_jar . tag  .= tag1
>     jarAt id_jar . zzz  .= zzz1
 
> mark ::
>   (  HasAction, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasJars vat_w (Map (Id Jar) jar_w),
>                       HasTag jar_w wad,
>                       HasZzz jar_w nat)
>   => Id Jar -> wad -> nat -> m ()
 
\subsection{|tell| --- update collateral market price}

> tell x =
>   auth . note $ do
>     vat . fix .= x
 
> tell ::
>   (  HasAction, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasFix vat_w wad)
>   => wad -> m ()
 
 
\section{Acts performed by keepers}

\subsection{|drip| -- update stability fee accumulator}

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

\subsection{|prod| --- adjust target price}

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
>   -- Sensitivity applied over time difference
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

> prod ::
>   (  HasAction, Logs m,
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
 
\section{Acts performed by tests}

\subsection{|warp| --- travel in time}

> warp t =
>   auth . note $ do
>     era += t

> warp ::
>   (  HasAction, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasEra w nat,
>                     Num nat)
>   => nat -> m ()
 
\section{Acts performed by tokens}
 
> pull id_jar id_lad w = do
>   g   <- need (jarAt id_jar . gem)
>   g'  <- transferFrom id_lad (Id "Vat") w g
>   jarAt id_jar . gem .= g'

> pull ::
>   (  Fails m,
>      Reads r m,
>        HasVat r vat_r,  HasJars vat_r (Map (Id Jar) jar_r),
>                           HasGem jar_r Gem,
>      Writes w m,
>        HasVat w vat_w,  HasJars vat_w (Map (Id Jar) jar_r))
>   => Id Jar -> Id Lad -> Wad -> m ()

\section{System model actions}
 
> newLad id_lad = lads.at id_lad ?= Lad
 
> newLad ::
>   ( Writes w m, HasLads w (IdMap Lad))
>   => Id Lad -> m ()
 
 
> newJar id id_jar =
>   auth . note $ do
>     vat . jars .at id ?= id_jar

> newJar ::
>   (  HasAction, Fails m, Logs m,
>      Reads r m,   HasSender r (Id Lad),
>      Writes w m,  HasVat w vat_w,
>                     HasJars vat_w (IdMap Jar))
>   =>
>     Id Jar -> Jar -> m ()
 
\section{Other stuff}

> perform :: Action -> Maker ()
> perform x =
>   let ?action = x in case x of
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

\end{document}

