\documentclass[twoside,12pt]{report}

\usepackage[a4paper]{geometry}
\usepackage[T1]{fontenc}
\usepackage{amsmath}
\usepackage{scalefnt}
\usepackage{amssymb}
\usepackage{graphicx}
\usepackage[hidelinks]{hyperref}
\usepackage{xcolor}
\usepackage{parskip}
\usepackage{tocloft}
\usepackage{mathtools}

\hypersetup{colorlinks, linkcolor={blue!45!red}}

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

%format art = "\texttt{art}"
%format Art = "\texttt{Art}"
%format axe = "\texttt{axe}"
%format Axe = "\texttt{Axe}"
%format cap = "\texttt{cap}"
%format Cap = "\texttt{Cap}"
%format cat = "\texttt{cat}"
%format Cat = "\texttt{Cat}"
%format chi = "\texttt{chi}"
%format Chi = "\texttt{Chi}"
%format con = "\texttt{con}"
%format Con = "\texttt{Con}"
%format cow = "\texttt{cow}"
%format Cow = "\texttt{Cow}"
%format din = "\texttt{din}"
%format Din = "\texttt{Din}"
%format era = "\texttt{era}"
%format Era = "\texttt{Era}"
%format fix = "\texttt{fix}"
%format Fix = "\texttt{Fix}"
%format gem = "\texttt{gem}"
%format Gem = "\texttt{Gem}"
%format hat = "\texttt{hat}"
%format Hat = "\texttt{Hat}"
%format how = "\texttt{how}"
%format How = "\texttt{How}"
%format ilk = "\texttt{ilk}"
%format Ilk = "\texttt{Ilk}"
%format jam = "\texttt{jam}"
%format Jam = "\texttt{Jam}"
%format jar = "\texttt{jar}"
%format Jar = "\texttt{Jar}"
%format lad = "\texttt{lad}"
%format Lad = "\texttt{Lad}"
%format lag = "\texttt{lag}"
%format Lag = "\texttt{Lag}"
%format mat = "\texttt{mat}"
%format Mat = "\texttt{Mat}"
%format min = "\texttt{min}"
%format Min = "\texttt{Min}"
%format sec = "\texttt{sec}"
%format Sec = "\texttt{Sec}"
%format par = "\texttt{par}"
%format Par = "\texttt{Par}"
%format phi = "\texttt{phi}"
%format Phi = "\texttt{Phi}"
%format joy = "\texttt{joy}"
%format Joy = "\texttt{Joy}"
%format rum = "\texttt{rum}"
%format Rum = "\texttt{Rum}"
%format pro = "\texttt{pro}"
%format Pro = "\texttt{Pro}"
%format ray = "\texttt{ray}"
%format Ray = "\texttt{Ray}"
%format rho = "\texttt{rho}"
%format Rho = "\texttt{Rho}"
%format sin = "\texttt{sin}"
%format Sin = "\texttt{Sin}"
%format sys = "\texttt{sys}"
%format Sys = "\texttt{Sys}"
%format tab = "\texttt{tab}"
%format Tab = "\texttt{Tab}"
%format tag = "\texttt{tag}"
%format Tag = "\texttt{Tag}"
%format tau = "\texttt{tau}"
%format Tau = "\texttt{Tau}"
%format tax = "\texttt{tax}"
%format Tax = "\texttt{Tax}"
%format urn = "\texttt{urn}"
%format Urn = "\texttt{Urn}"
%format vat = "\texttt{vat}"
%format Vat = "\texttt{Vat}"
%format vow = "\texttt{vow}"
%format Vow = "\texttt{Vow}"
%format wad = "\texttt{wad}"
%format Wad = "\texttt{Wad}"
%format way = "\texttt{way}"
%format Way = "\texttt{Way}"
%format zzz = "\texttt{zzz}"
%format Zzz = "\texttt{Zzz}"

%format urns = "\texttt{urn}s"
%format jars = "\texttt{jar}s"
%format ilks = "\texttt{ilk}s"

%format art0
%format art1
%format axe0
%format axe1
%format cat0
%format chi0
%format chi1
%format con0
%format con1
%format cow0
%format cow1
%format era0
%format fix0
%format hat0
%format hat1
%format how0
%format how1
%format ilk0
%format jar0
%format joy0
%format joy1
%format lag0
%format lag1
%format mat0
%format mat1
%format par0
%format par1
%format pro0
%format rho0
%format sys0
%format sys1
%format tag0
%format tag1
%format tau0
%format tax0
%format tax1
%format urn0
%format vow0
%format wad0
%format wad_chi
%format wad_dai
%format wad_gem
%format wad_mkr
%format way0
%format way1
%format zzz0
%format zzz1

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
%format Sec = "\texttt{Sec}"

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
%format chop = "\texttt{chop}"
%format Chop = "\texttt{Chop}"
%format cork = "\texttt{cork}"
%format Cork = "\texttt{Cork}"
%format calm = "\texttt{calm}"
%format Calm = "\texttt{Calm}"
%format cuff = "\texttt{cuff}"
%format Cuff = "\texttt{Cuff}"
%format crop = "\texttt{crop}"
%format Crop = "\texttt{Crop}"
%format give = "\texttt{give}"
%format Give = "\texttt{Give}"
%format grab = "\texttt{grab}"
%format Grab = "\texttt{Grab}"
%format heal = "\texttt{heal}"
%format Heal = "\texttt{Heal}"
%format lock = "\texttt{lock}"
%format Lock = "\texttt{Lock}"
%format love = "\texttt{love}"
%format Love = "\texttt{Love}"
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
\listoftables
\listoffigures

\clearpage

\chapter{Introduction}

\newcommand{\MakerDAO}{\textsc{dai maker}}
\newcommand{\actentry}[2]
  {\phantomsection\addcontentsline{toc}{subsection}{#1 --- #2}}
\newcommand{\subactentry}[2]
  {\phantomsection\addcontentsline{toc}{subsubsection}{#1 --- #2}}
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
read previously unwritten storage and get back a zero value, whereas
in Haskell we must give explicit defaults.  The state rollback
behavior of failed actions is also explicit in the type of the
execution function, which may return an error.

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

We replace the default prelude module with our own.  This brings in
dependencies and hides unneeded symbols.
Consult Appendix~\ref{appendix:prelude} to see exactly what is brought
into scope.

> module Maker where
>
> import Maker.Prelude   -- Fully import the Maker prelude
> import Prelude ()      -- Import nothing from Prelude
>
> import Maker.Decimal

\chapter{Types}

\section{Numeric types}

The system uses two different precisions of decimal fixed point
numbers, which we call \emph{wads} and \emph{rays}, having
respectively 18 digits of precision (used for token quantities) and 36
digits (used for precise rates and ratios).

> -- Define the distinct |wad| type for currency quantities
> newtype Wad = Wad (Decimal E18)
>   deriving (  Ord, Eq, Num, Real, Fractional, RealFrac)
>
> -- Define the distinct |ray| type for precise rate quantities
> newtype Ray = Ray (Decimal E36)
>   deriving (  Ord, Eq, Num, Real, Fractional, RealFrac)

See Appendix~\ref{appendix:numbers} for details on how we modify
Haskell's decimal fixed point type to do more correct rounding for
multiplication and division.

%if 0

> instance Read Ray where
>   readsPrec n s = fmap (first Ray) (readsPrec n s)
> instance Read Wad where
>   readsPrec n s = fmap (first Wad) (readsPrec n s)
> instance Read Sec where
>   readsPrec n s = fmap (first Sec) (readsPrec n s)
>
> instance Show Wad  where show (Wad x)  = show x
> instance Show Ray  where show (Ray x)  = show x
> instance Show Sec  where show (Sec x)  = show x

%endif

Haskell number types are not automatically converted, so in
calculations that combine wads and rays, we convert explicitly with a
|cast| function.

> -- Convert via fractional $n/m$ form.
> cast :: (Real a, Fractional b) => a -> b
> cast = fromRational . toRational

We also define a type for time durations in whole seconds.

> newtype Sec = Sec Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)

> instance Epsilon Wad  where epsilon = Wad epsilon
> instance Epsilon Ray  where epsilon = Ray epsilon

\section{Identifiers and addresses}

There are several kinds of identifiers used in the system, and we can
use types to distinguish them.

> -- The type parameter |a| creates distinct types.
> -- For example, |Id Foo| and |Id Bar| are incompatible.
>
> data Id a = Id String
>   deriving (Show, Eq, Ord)

We define another type for representing Ethereum account addresses.

> data Address = Address String
>   deriving (Ord, Eq, Show)

We also have three predefined entity identifiers.

> -- The |dai| token address
> id_dai = Id "Dai"
>
> -- The |cdp| engine address
> id_vat = Address "Vat"
>
> -- The account with ultimate authority
> -- \xxx{Kludge until authority is modelled}
> id_god = Address "God"

%if 0

> instance Read (Id a) where
>   readsPrec n s = fmap (first Id) (readsPrec n s)

%endif

This section introduces the records stored by the Maker system.

\section{|Gem| --- ERC20 token model}

> data Gem = Gem {
> 
>     gemTotalSupply  :: Wad,
>     gemBalanceOf    :: Map Address             Wad,
>     gemAllowance    :: Map (Address, Address)  Wad
>
>   } deriving (Eq, Show)
>

\section{|Jar| --- collateral type}

\actentry{|gem|}{collateral token}
\actentry{|tag|}{market price of token}
\actentry{|zzz|}{expiration date of token price feed}

> data Jar = Jar {
>
>     jarGem  :: Gem,  -- Collateral token
>     jarTag  :: Wad,  -- Market price
>     jarZzz  :: Sec   -- Price expiration
>
>   } deriving (Eq, Show)

\section{|Ilk| --- |cdp| type}

\actentry{|jar|}{collateral token vault}
\actentry{|mat|}{liquidation ratio}
\actentry{|axe|}{liquidation penalty ratio}
\actentry{|hat|}{debt ceiling}
\actentry{|tax|}{stability fee}
\actentry{|lag|}{price feed limbo duration}
\actentry{|rho|}{timestamp of last drip}
\actentry{|din|}{total outstanding dai}
\actentry{|chi|}{price of debt coin for |cdp| type}

> data Ilk = Ilk {
>
>     ilkJar  :: Id Jar,  -- Collateral vault
>     ilkAxe  :: Ray,     -- Liquidation penalty
>     ilkHat  :: Wad,     -- Debt ceiling
>     ilkMat  :: Ray,     -- Liquidation ratio
>     ilkTax  :: Ray,     -- Stability fee
>     ilkLag  :: Sec,     -- Limbo duration
>     ilkRho  :: Sec,     -- Last dripped
>     ilkRum  :: Wad,     -- Total debt in debt unit
>     ilkChi  :: Ray      -- Value of debt unit
>
>   } deriving (Eq, Show)

\section{|Urn| --- collateralized debt position (|cdp|)}

\actentry{|cat|}{address of liquidation initiator}
\actentry{|vow|}{address of liquidation contract}
\actentry{|lad|}{|dai| issuer / |cdp| owner}
\actentry{|ilk|}{|cdp| type}
\actentry{|art|}{debt denominated in debt units}
\actentry{|jam|}{collateral denominated in debt units}

> data Urn = Urn {
>   
>     urnCat  :: Maybe Address,  -- Address of liquidation initiator
>     urnVow  :: Maybe Address,  -- Address of liquidation contract
>     urnLad  :: Address,        -- Issuer
>     urnIlk  :: Id Ilk,         -- |cdp| type
>     urnArt  :: Wad,            -- Outstanding debt in debt units
>     urnJam  :: Wad             -- Collateral amount in debt units
>
>   } deriving (Eq, Show)

\section{|Vat| --- |cdp| engine}

\actentry{|fix|}{market price of |dai| denominated in |sdr|}
\actentry{|par|}{target price of |dai| denominated in |sdr|}
\actentry{|how|}{sensitivity parameter}
\actentry{|way|}{rate of target price change}
\actentry{|tau|}{timestamp of last revaluation}
\actentry{|joy|}{unprocessed stability fee revenue}
\actentry{|sin|}{bad debt from liquidated |cdp|s}

> data Vat = Vat {
>
>     vatFix  :: Wad,                -- Market price
>     vatHow  :: Ray,                -- Sensitivity
>     vatPar  :: Wad,                -- Target price
>     vatWay  :: Ray,                -- Target rate
>     vatTau  :: Sec,                -- Last prodded
>     vatJoy  :: Wad,                -- Unprocessed stability fees
>     vatSin  :: Wad,                -- Bad debt from liquidated |cdp|s
>     vatJars  :: Map (Id Jar) Jar,  -- Collateral tokens
>     vatIlks  :: Map (Id Ilk) Ilk,  -- |cdp| types
>     vatUrns  :: Map (Id Urn) Urn   -- |cdp|s
>
>   } deriving (Eq, Show)

\section{System model}

\actentry{|era|}{Current timestamp}

> data System =  System {
> 
>     systemVat       :: Vat,       -- Root Maker entity
>     systemEra       :: Sec,       -- Current time stamp
>     systemSender    :: Address,   -- Sender of current act
>     systemAccounts  :: [Address]  -- For test suites
>
>   } deriving (Eq, Show)

\section*{Lens fields}

> makeFields ''Gem
> makeFields ''Jar
> makeFields ''Ilk
> makeFields ''Urn
> makeFields ''Vat
> makeFields ''System

\section{Default data}

> defaultIlk :: Id Jar -> Ilk
> defaultIlk id_jar = Ilk {
>   ilkJar  = id_jar,
>   ilkAxe  = Ray 1,
>   ilkMat  = Ray 1,
>   ilkTax  = Ray 1,
>   ilkHat  = Wad 0,
>   ilkLag  = Sec 0,
>   ilkChi  = Ray 1,
>   ilkRum  = Wad 0,
>   ilkRho  = Sec 0
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
>   vatJoy   = Wad 0,
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
>   systemVat       = initialVat how0,
>   systemEra       = 0,
>   systemSender    = id_god,
>   systemAccounts  = mempty
> }

\chapter{Acts}

The \emph{acts} are the basic state transitions of the credit system.

For details on the underlying ``Maker monad,'' which specifies how the
act definitions behave with regard to state and rollback thereof, see
chapter~\ref{chapter:monad}.

\newpage
\section{Risk assessment}

\actentry{|gaze|}{identify |cdp| risk stage}

We divide an urn's situation into five stages of risk.
Table \ref{table:stages} shows which acts each stage allows.
The stages are naturally ordered from more to less risky.

> data Stage  =  Dread | Grief | Panic | Worry | Anger | Pride
>
>   deriving (Eq, Ord, Show)

First we define a pure function |analyze| that determines an
urn's stage.

> analyze era0 par0 urn0 ilk0 jar0 =
>   if
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
>   -- Safely overcollateralized
>     | otherwise                                 -> Pride
>
>   where
>   -- |cdp|'s collateral value in |sdr|:
>     pro  = view jam urn0  * view tag jar0
>
>   -- |cdp| type's total debt in |sdr|:
>     cap  = (view rum ilk0  * cast (view chi ilk0)) :: Wad
>
>   -- |cdp|'s debt in |sdr|:
>     con  = view art urn0  * cast (view chi ilk0) * par0
>
>   -- Required collateral as per liquidation ratio:
>     min  = con * view mat ilk0
>

\clearpage

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

Now we define the internal act |gaze| which returns the value of
|analyze| after ensuring the system state is updated.

> gaze id_urn = do
> -- Perform dai revaluation and rate adjustment
>   prod
>
> -- Update price of specific debt unit
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   drip id_ilk
>
> -- Read parameters for risk analysis
>   era0    <- view era
>   par0    <- view (vat . par)
>   urn0    <- look (vat . urns . ix id_urn)
>   ilk0    <- look (vat . ilks . ix (view ilk urn0  ))
>   jar0    <- look (vat . jars . ix (view jar ilk0  ))
>
> -- Return risk stage of |cdp|
>   return (analyze era0 par0 urn0 ilk0 jar0)

\section{Lending}

\actentry{|open|}{create |cdp| account}

> open id_urn id_ilk =
>   do
>     id_lad <- view sender
>     vat . urns . at id_urn ?= defaultUrn id_ilk id_lad

\actentry{|lock|}{deposit collateral}

> lock id_urn x = do
>
>   -- Ensure |cdp| exists; identify collateral type
>     id_ilk  <- look (vat . urns . ix id_urn  . ilk)
>     id_jar  <- look (vat . ilks . ix id_ilk  . jar)
>
>   -- Record an increase in collateral
>     vat . urns . ix id_urn . jam += x
>
>   -- Take sender's tokens
>     id_lad  <- view sender
>     pull id_jar id_lad x

\actentry{|free|}{withdraw collateral}

> free id_urn wad_gem = do
>
>   -- Fail if sender is not the |cdp| owner
>     id_sender  <- view sender
>     id_lad     <- look (vat . urns . ix id_urn . lad)
>     aver (id_sender == id_lad)
>
>   -- Decrease the collateral amount
>     vat . urns . ix id_urn . jam  -=  wad_gem
>
>   -- Roll back if undercollateralized
>     gaze id_urn >>= aver . (== Pride)
>
>   -- Send the collateral to the |cdp| owner
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     id_jar  <- look (vat . ilks . ix id_ilk . jar)
>     push id_jar id_lad wad_gem

\actentry{|draw|}{issue dai as debt}

> draw id_urn wad_dai = do
>
>   -- Fail if sender is not the |cdp| owner
>     id_sender  <- view sender
>     id_lad     <- look (vat . urns . ix id_urn . lad)
>     aver (id_sender == id_lad)
>
>   -- Update value of debt unit
>     id_ilk     <- look (vat . urns . ix id_urn . ilk)
>     chi1       <- drip id_ilk
>
>   -- Denominate draw amount in debt unit
>     let  wad_chi = wad_dai / cast chi1
>
>   -- Increase debt
>     vat . urns . ix id_urn . art += wad_chi
>
>   -- Roll back unless overcollateralized
>     gaze id_urn >>= aver . (== Pride)
>
>   -- Mint dai and send to the |cdp| owner
>     mint id_dai wad_dai
>     push id_dai id_lad wad_dai

\actentry{|wipe|}{repay debt and burn dai}

> wipe id_urn wad_dai = do
>
>   -- Fail if sender is not the |cdp| owner
>     id_sender  <- view sender
>     id_lad     <- look (vat . urns . ix id_urn . lad)
>     aver (id_sender == id_lad)
>
>   -- Update value of debt unit
>     id_ilk <- look (vat . urns . ix id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Roll back unless overcollateralized
>     gaze id_urn >>= aver . (== Pride)
>
>   -- Denominate dai amount in debt unit
>     let  wad_chi = wad_dai / cast chi1
> 
>   -- Reduce debt
>     vat . urns . ix id_urn . art -= wad_chi
>
>   -- Take dai from |cdp| owner, or roll back
>     pull id_dai id_lad wad_dai
>
>   -- Destroy dai
>     burn id_dai wad_dai

\actentry{|give|}{transfer |cdp| account}

> give id_urn id_lad = do
> 
>     x <- look (vat . urns . ix id_urn . lad)
>     y <- view sender
>     aver (x == y)
>     vat . urns . ix id_urn . lad .= id_lad

\actentry{|shut|}{wipe, free, and delete |cdp|}

> shut id_urn = do
>
>   -- Update value of debt unit
>     id_ilk <- look (vat . urns . ix id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Attempt to repay all the |cdp|'s outstanding dai
>     art0 <- look (vat . urns . ix id_urn . art)
>     wipe id_urn (art0 * cast chi1)
>
>   -- Reclaim all the collateral
>     jam0 <- look (vat . urns . ix id_urn . jam)
>     free id_urn jam0
>
>   -- Nullify the |cdp|
>     vat . urns . at id_urn .= Nothing

\clearpage
\section{Frequent adjustments}

\actentry{|prod|}{perform revaluation and rate adjustment}

> prod = do
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
>     age  = era0 - tau0
>
>   -- Current target rate applied to target price
>     par1  = par0 * cast (way0 ^^ age)
>
>   -- Sensitivity parameter applied over time
>     wag  = how0 * fromIntegral age
>
>   -- Target rate scaled up or down
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

\clearpage
\actentry{|drip|}{update value of debt unit}

> drip id_ilk = do
>
> -- Current time stamp
>   era0  <- view era
>
> -- Time stamp of previous |drip|
>   rho0  <- look (vat . ilks . ix id_ilk . rho)
>
> -- Current stability fee
>   tax0  <- look (vat . ilks . ix id_ilk . tax)
>
> -- Current value of debt unit
>   chi0  <- look (vat . ilks . ix id_ilk . chi)
>
> -- Current total debt in debt units
>   rum0  <- look (vat . ilks . ix id_ilk . rum)
>
> -- Current unprocessed stability fee revenue
>   joy0  <- look (vat . ilks . ix id_ilk . joy)
>
>   let
>
>     age   = era0 - rho0
>
>     chi1  = chi0 * tax0 ^^ age
>
>     joy1  = joy0 + (cast (chi1 - chi0) :: Wad) * rum0
>
>   vat . ilks . ix id_ilk . chi  .= chi1
>   vat . ilks . ix id_ilk . rho  .= era0
>   vat . ilks . ix id_ilk . joy  .= joy1
>
>   return chi1

\section{Price feedback}

\actentry{|mark|}{update market price of dai}

> mark id_jar tag1 zzz1 =
>   auth $ do
>     jarAt id_jar . tag  .= tag1
>     jarAt id_jar . zzz  .= zzz1

\actentry{|tell|}{update market price of collateral token}

> tell x =
>   auth $ do
>     vat . fix .= x

\section{Liquidation and settlement}

\actentry{|bite|}{mark |cdp| for liquidation}

> bite id_urn = do
>
>   -- Fail if |cdp| is not in need of liquidation
>     gaze id_urn >>= aver . (== Panic)
>
>   -- Record the sender as the liquidation initiator
>     id_cat              <- view sender
>     vat . urns . ix id_urn . cat  .= id_cat
>
>   -- Read current debt
>     art0    <- look (vat . urns . ix id_urn  . art)
>
>   -- Update value of debt unit
>     id_ilk     <- look (vat . urns . ix id_urn . ilk)
>     chi1       <- drip id_ilk
>
>   -- Read liquidation penalty ratio
>     id_ilk  <- look (vat . urns . ix id_urn  . ilk)
>     axe0    <- look (vat . ilks . ix id_ilk  . axe)
>
>   -- Apply liquidation penalty to debt
>     let art1 = art0 * axe0
>
>   -- Update |cdp| debt
>     vat . urns . ix id_urn . art   .=  art1
>
>   -- Record as bad debt
>     sin +=  art1 * chi1

\actentry{|grab|}{take tokens to begin |cdp| liquidation}

> grab id_urn =
>
>   auth $ do
>
>   -- Fail if |cdp| is not marked for liquidation
>     gaze id_urn >>= aver . (== Grief)
>
>   -- Record the sender as the |cdp|'s settler
>     id_vow <- view sender
>     vat . urns . ix id_urn . vow .= id_vow
>
>   -- Clear the |cdp|'s requester of liquidation
>     vat . urns . ix id_urn . cat .= Nothing

\actentry{|heal|}{process bad debt}

> heal wad_dai =
>
>   auth $ do
>
>     vat . sin -= wad_dai

\actentry{|love|}{process stability fee revenue}

> love wad_dai =
>
>   auth $ do
>
>     vat . joy -= wad_dai

\section{Governance}

\actentry{|form|}{create a new |cdp| type}

> form id_ilk id_jar =
>   auth $ do
>     vat . ilks . at id_ilk ?= defaultIlk id_jar

\actentry{|frob|}{set the sensitivity parameter}

> frob how' =
>   auth $ do
>     vat . how .= how'

\actentry{|chop|}{set liquidation penalty}

> chop id_ilk axe1 =
>   auth $ do
>     vat . ilks . ix id_ilk . axe .= axe1

\actentry{|cork|}{set debt ceiling}

> cork id_ilk hat1 =
>   auth $ do
>     vat . ilks . ix id_ilk . hat .= hat1

\actentry{|calm|}{set limbo duration}

> calm id_ilk lag1 =
>   auth $ do
>     vat . ilks . ix id_ilk . lag .= lag1

\actentry{|cuff|}{set liquidation ratio}

> cuff id_ilk mat1 =
>   auth $ do
>     vat . ilks . ix id_ilk . mat .= mat1

\actentry{|crop|}{set stability fee}

> crop id_ilk tax1 =
>   auth $ do
>     drip id_ilk
>     vat . ilks . ix id_ilk . tax .= tax1

\section{Minting, burning, and transferring}

\actentry{|pull|}{take tokens to vat}

> pull id_jar id_lad w = do
>   g   <- look (jarAt id_jar . gem)
>   g'  <- transferFrom id_lad id_vat w g
>   jarAt id_jar . gem .= g'

\actentry{|push|}{send tokens from vat}

> push id_jar id_lad w = do
>   g   <- look (jarAt id_jar . gem)
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
>   auth $ do
>     era += t

\actentry{|mine|}{create new collateral token}

> mine id_jar = do
>     vat . jars . at id_jar ?= Jar {
>       jarGem   = Gem {
>         gemTotalSupply  = 1000000000000,
>         gemBalanceOf    = singleton id_vat 1000000000000,
>         gemAllowance    = empty
>       },
>       jarTag  = Wad 0,
>       jarZzz  = 0
>     }

\actentry{|hand|}{give collateral tokens to account}

> hand dst wad id_jar = do
>   push id_jar dst wad

\actentry{|sire|}{create a new account}

> sire lad = do
>   accounts %= (lad :)

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
>     Mine id          -> mine id
>     Hand lad wad jar -> hand lad wad jar
>     Sire lad         -> sire lad
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

\chapter{Act framework}
\label{chapter:monad}

\section{Act descriptions}

We define the Maker act vocabulary as a data type.

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
>   |  Love     Wad
>   |  Mark     (Id Jar)  Wad       Sec
>   |  Open     (Id Urn)  (Id Ilk)
>   |  Prod
>   |  Pull     (Id Jar)  Address   Wad
>   |  Shut     (Id Urn)
>   |  Tell     Wad
>   |  Warp     Sec
>   |  Wipe     (Id Urn)  Wad
>   |  Mine     (Id Jar)
>   |  Hand     Address   Wad       (Id Jar)
>   |  Sire     Address
>
> -- Test acts
>   |  Addr     Address
> 
>   deriving (Eq, Show)

Acts can fail.  We divide the failure modes into general assertion
failures and authentication failures.

> data Error = AssertError | AuthError
>   deriving (Show, Eq)

\section{The |Maker| monad}
\label{section:maker-monad}

The reader does not need any abstract understanding of monads to
understand the code.  What they give us is a nice syntax---the |do|
notation---for expressing exceptions and state in a way that is still
purely functional.

> newtype Maker a =
>   Maker (StateT System (Except Error) a)
>
>   deriving
>     (  Functor, Applicative, Monad,
>        MonadError   Error,
>        MonadState   System)

> exec  ::  System
>       ->  Maker ()
>       ->  Either Error System
> exec sys (Maker m) =
>   runExcept (execStateT m sys)

The following instance makes the mutable state also available as
read-only state.

> instance MonadReader System Maker where
>   ask = Maker get
>   local f (Maker m) = Maker $ do
>     s <- get;  put (f s)
>     x <- m;    put s
>     return x

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

\section{Asserting}

> aver :: Fails m => Bool -> m ()
> aver x = unless x (throwError AssertError)
>
> look :: (Fails m, Reads r m)
>      => Getting (First a) r a -> m a
> look f = preview f >>= \case
>   Nothing -> throwError AssertError
>   Just x  -> return x

\section{Modifiers}

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


\chapter{Testing}

Sketches for property stuff...

> data Parameter =
>      Fix | Par | Way

> maintains
>   :: Eq a  => Lens' System a -> Maker ()
>            -> System -> Bool
> 
> maintains p = \ m sys0 ->
>   case exec sys0 m of
>   -- On success, data must be compared for equality
>     Right sys1  -> view p sys0 == view p sys1
>   -- On rollback, data is maintained by definition
>     Left _      -> True

> changesOnly
>   ::  Lens' System a -> Maker ()
>   ->  System -> Bool
> 
> changesOnly p = \m sys0 ->
>   case exec sys0 m of
>   -- On success, equalize |p| and compare
>     Right sys1  -> set p (view p sys1) sys0 == sys1
>   -- On rollback, data is maintained by definition
>     Left _      -> True

> also :: Lens' s a -> Lens' s b -> Lens' s (a, b)
> also f g = lens getter setter
>   where
>   getter x = (view f x, view g x)
>   setter x (a, b) = set f a (set g b x)

> keeps :: Parameter -> Maker () -> System -> Bool
> keeps Fix  = maintains (vat . fix)
> keeps Par  = maintains (vat . par)
> keeps Way  = maintains (vat . way)

Thus:

> foo sys0 = all (\f -> f sys0)
>   [changesOnly (  (vat . par) `also`
>                   (vat . way))
>      (perform Prod)]

\appendix

\chapter{Act type signatures}
\label{appendix:types}

> type Reads   r  m = MonadReader r m
> type Writes  w  m = MonadState w m
> type Fails      m = MonadError Error m
>
> type IsAct    = ?act :: Act

> type Numbers wad ray sec =
>   (wad ~ Wad, ray ~ Ray, sec ~ Sec)

We see that |drip| may fail; it reads an |ilk|'s |tax|, |cow|, |rho|,
and |bag|; and it writes those same parameters except |tax|.

> drip ::
>   (  Fails m,
>      Reads r m,
>        HasEra r Sec,
>        HasVat r vat_r,
>          HasIlks vat_r (Map (Id Ilk) ilk_r),
>            HasTax ilk_r Ray,
>            HasRho ilk_r Sec,
>            HasChi ilk_r Ray,
>            HasRum ilk_r Wad,
>            HasJoy ilk_r Wad,
>      Writes w m,
>        HasVat w vat_w,
>          HasIlks vat_w (Map (Id Ilk) ilk_w),
>            HasRho ilk_w Sec,
>            HasJoy ilk_w Wad,
>            HasChi ilk_w Ray)
>   => Id Ilk -> m Ray


> form ::
>
>   (  IsAct, Fails m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasIlks vat_w (Map (Id Ilk) Ilk))
>
>  => Id Ilk -> Id Jar -> m ()

> frob :: (  IsAct, Fails m,
>            Reads r m,   HasSender r Address,
>            Writes w m,  HasVat w vat_w,
>                           HasHow vat_w ray)
>   => ray -> m ()

> open ::
>   (  IsAct,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasUrns vat_w (Map (Id Urn) Urn))
>   => Id Urn -> Id Ilk -> m ()

> give ::
>   (  IsAct, Fails m,
>      Reads r m,   HasSender r Address,
>                   HasVat r vat_r,
>                     HasUrns vat_r (Map (Id Urn) urn_r),
>                       HasLad urn_r Address,
>      Writes w m,  HasVat w vat_r)
>   => Id Urn -> Address -> m ()

> lock ::
>   (  IsAct, Fails m,
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
>   (  IsAct, Fails m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasJars vat_w (Map (Id Jar) jar_w),
>                       HasTag jar_w wad,
>                       HasZzz jar_w sec)
>   => Id Jar -> wad -> sec -> m ()

> tell ::
>   (  IsAct, Fails m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasVat w vat_w,
>                     HasFix vat_w wad)
>   => wad -> m ()

> prod ::
>   (  IsAct,
>      Reads r m,
>        HasSender r Address,
>        HasEra r sec,
>        HasVat r vat_r,  (  HasPar vat_r wad,
>                            HasTau vat_r sec,
>                            HasHow vat_r ray,
>                            HasWay vat_r ray,
>                            HasFix vat_r wad),
>      Writes w m,
>        HasVat w vat_w,  (  HasPar vat_w wad,
>                            HasWay vat_w ray,
>                            HasTau vat_w sec),
>      Integral sec,
>      Ord wad, Fractional wad,
>      Fractional ray, Real ray)
>   => m ()

> warp ::
>   (  IsAct, Fails m,
>      Reads r m,   HasSender r Address,
>      Writes w m,  HasEra w sec,
>                     Num sec)
>   => sec -> m ()


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
>   (  IsAct, Fails m,
>      Numbers wad ray sec,
>      Reads r m,
>        HasSender r Address,
>        HasEra r Sec,
>        HasVat r vat_r,
>          HasFix vat_r wad,
>          HasPar vat_r wad,
>          HasHow vat_r ray,
>          HasWay vat_r ray,
>          HasTau vat_r sec,
>          HasUrns vat_r (Map (Id Urn) urn_r),
>            HasJam urn_r wad,
>            HasArt urn_r wad,
>            HasCat urn_r (Maybe Address),  HasVow urn_r (Maybe Address),
>            HasIlk urn_r (Id Ilk),
>          HasIlks vat_r (Map (Id Ilk) ilk_r),
>            HasHat ilk_r wad,
>            HasMat ilk_r wad,
>            HasRum ilk_r wad,
>            HasJoy ilk_r wad,
>            HasTax ilk_r ray,
>            HasLag ilk_r sec,
>            HasChi ilk_r ray,  HasRho ilk_r sec,
>            HasJar ilk_r (Id Jar),
>          HasJars vat_r (Map (Id Jar) jar_r),
>            HasGem jar_r Gem,
>            HasTag jar_r wad,
>            HasZzz jar_r sec,
>      Writes w m,
>        HasVat w vat_w,
>          HasTau vat_w sec,
>          HasWay vat_w ray,  HasPar vat_w wad,
>          HasUrns vat_w (Map (Id Urn) urn_w),
>            HasJam urn_w wad,  HasArt urn_w wad,
>            HasVow urn_w Address,
>            HasCat urn_w (Maybe Address),
>          HasIlks vat_w (Map (Id Ilk) ilk_w),
>            HasJoy ilk_w wad,
>            HasChi ilk_w ray,
>            HasRho ilk_w sec,
>          HasJars vat_w (Map (Id Jar) jar_r)
>   ) => Id Urn -> m ()
>

%include Maker/Prelude.lhs
%include Maker/Decimal.lhs

\end{document}
