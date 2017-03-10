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

%include maker.fmt

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

\par \vspace{2.5cm}
\addtolength{\tabcolsep}{-4pt}
\addtolength{\extrarowheight}{4pt}
{\large\sffamily
\begin{tabular}{ r l }
Daniel & Brockman \\ Mikael & Brockman \\ Nikolai & Mushegian
\end{tabular}
\addtolength{\extrarowheight}{-4pt}
\addtolength{\tabcolsep}{4pt}}

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

\item \textbf{Typing.}  While Solidity does have a static
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

\section{Limitations}

This implementation has a simplified model of Maker's governance
authorization.  Instead of the ``access control list'' approach of the
\texttt{DSGuard} component, we give full authority to one single
address.  A future iteration will include the full
authorization model.

We also do not currently model the EVM's 256 bit word size, but allow
all quantities to grow arbitrarily large.  This will also be modelled
in a future iteration.

Finally, our model of |ERC20| tokens is simplified, and for example
does not include the concept of ``allowances.''

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
> import Prelude ()      -- Import nothing from Prelude
> import Maker.Prelude   -- Import everything from Maker Prelude

We also import our definition of decimal fixed point numbers, listed
in Appendix~\ref{appendix:numbers}.

> import Maker.Decimal

%if 0

> import Debug.Trace

%endif

\chapter{Types}

\section{Numeric types}

The system uses two different precisions of decimal fixed point
numbers, which we call \emph{wads} and \emph{rays}, having
respectively 18 digits of precision (used for token quantities) and 36
digits (used for precise rates and ratios).
See Appendix~\ref{appendix:numbers} for details on decimal fixed point
numbers and rounding.

> -- Define the distinct type for currency quantities
> newtype Wad = Wad (Decimal E18)
>   deriving (  Ord, Eq, Num, Real, Fractional, RealFrac)
>
> -- Define the distinct type for rates and ratios
> newtype Ray = Ray (Decimal E36)
>   deriving (  Ord, Eq, Num, Real, Fractional, RealFrac)

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

> instance Epsilon Wad  where epsilon = Wad epsilon
> instance Epsilon Ray  where epsilon = Ray epsilon

%endif
We also define a type for time durations in whole seconds.

> newtype Sec = Sec Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)

Haskell number types are not automatically converted, so we convert
explicitly with a |cast| function.

> -- Convert via fractional $n/m$ form.
> cast :: (Real a, Fractional b) => a -> b
> cast = fromRational . toRational


\section{Identifiers and addresses}

There are several kinds of identifiers used in the system, and we use
types to distinguish them.

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
> id_dai = Id "DAI"
>
> -- The |cdp| engine address
> id_vat = Address "VAT"
>
> -- A test account with ultimate authority
> id_god = Address "GOD"

%if 0

> instance Read (Id a) where
>   readsPrec n s = fmap (first Id) (readsPrec n s)

%endif

\section{|Gem| --- token model}

In this model, all tokens behave in the same simple way.\footnote{In
the real world, token semantics can differ, despite nominally
following the |ERC20| interface.  Maker governance therefore involves
due diligence on collateral token contracts.}  We omit the |ERC20|
concept of ``allowances.''

> data Gem = Gem {
>
>     _balanceOf    :: Map Holder Wad
>
>   } deriving (Eq, Show)

We distinguish between tokens held by vaults, tokens held by the test
driver, and tokens held by |cdp| owners.

> data Holder  =  InAccount  Address
>              |  InVault    (Id Jar)
>              |  InToy
>
>   deriving (Eq, Show, Ord)


\section{|Jar| --- collateral vaults}

\actentry{|gem|}{collateral token}
\actentry{|tag|}{market price of token}
\actentry{|zzz|}{expiration time of token price feed}

> data Jar = Jar {
>
>     _gem  :: Gem,  -- Collateral token
>     _tag  :: Wad,  -- Market price
>     _zzz  :: Sec   -- Price expiration
>
>   } deriving (Eq, Show)

\section{|Ilk| --- |cdp| type}

\actentry{|jar|}{collateral token vault}
\actentry{|mat|}{liquidation ratio}
\actentry{|axe|}{liquidation penalty ratio}
\actentry{|hat|}{debt ceiling}
\actentry{|tax|}{stability fee}
\actentry{|lag|}{price feed limbo duration}
\actentry{|rho|}{time of debt unit adjustment}
\actentry{|din|}{total outstanding dai}
\actentry{|chi|}{dai value of debt unit}

> data Ilk = Ilk {
>
>     _jar  :: Id Jar,  -- Collateral vault
>     _axe  :: Ray,     -- Liquidation penalty
>     _hat  :: Wad,     -- Debt ceiling
>     _mat  :: Ray,     -- Liquidation ratio
>     _tax  :: Ray,     -- Stability fee
>     _lag  :: Sec,     -- Limbo duration
>     _rho  :: Sec,     -- Last dripped
>     _rum  :: Wad,     -- Total debt in debt unit
>     _chi  :: Ray      -- Dai value of debt unit
>
>   } deriving (Eq, Show)

\section{|Urn| --- collateralized debt position (|cdp|)}

\actentry{|cat|}{address of liquidation initiator}
\actentry{|vow|}{address of liquidation contract}
\actentry{|lad|}{|cdp| owner}
\actentry{|ilk|}{|cdp| type}
\actentry{|art|}{debt denominated in debt unit}
\actentry{|jam|}{collateral denominated in debt unit}

> data Urn = Urn {
>
>     _cat  :: Maybe Address,  -- Address of liquidation initiator
>     _vow  :: Maybe Address,  -- Address of liquidation contract
>     _lad  :: Address,        -- Issuer
>     _ilk  :: Id Ilk,         -- |cdp| type
>     _art  :: Wad,            -- Outstanding debt in debt unit
>     _jam  :: Wad             -- Collateral amount in debt unit
>
>   } deriving (Eq, Show)

\section{|Vat| --- |cdp| engine}

\actentry{|fix|}{market price of |dai| denominated in |sdr|}
\actentry{|par|}{target price of |dai| denominated in |sdr|}
\actentry{|how|}{sensitivity parameter}
\actentry{|way|}{rate of target price change}
\actentry{|tau|}{time of latest target update}
\actentry{|joy|}{unprocessed stability fee revenue}
\actentry{|sin|}{bad debt from liquidated |cdp|s}

> data Vat = Vat {
>
>     _fix  :: Wad,                -- Market price
>     _how  :: Ray,                -- Sensitivity
>     _par  :: Wad,                -- Target price
>     _way  :: Ray,                -- Target rate
>     _tau  :: Sec,                -- Last prodded
>     _joy  :: Wad,                -- Unprocessed stability fees
>     _sin  :: Wad,                -- Bad debt from liquidated |cdp|s
>     _jars  :: Map (Id Jar) Jar,  -- Collateral tokens
>     _ilks  :: Map (Id Ilk) Ilk,  -- |cdp| types
>     _urns  :: Map (Id Urn) Urn   -- |cdp|s
>
>   } deriving (Eq, Show)

\section{System model}

\actentry{|era|}{current time}

> data System =  System {
>
>     _vat       :: Vat,       -- Root Maker entity
>     _era       :: Sec,       -- Current time stamp
>     _sender    :: Address,   -- Sender of current act
>     _accounts  :: [Address]  -- For test suites
>
>   } deriving (Eq, Show)

\section*{Lens fields}

> makeLenses ''Gem
> makeLenses ''Jar
> makeLenses ''Ilk
> makeLenses ''Urn
> makeLenses ''Vat
> makeLenses ''System

\section{Default data}

> defaultIlk :: Id Jar -> Ilk
> defaultIlk id_jar = Ilk {
>   _jar  = id_jar,
>   _axe  = Ray 1,
>   _mat  = Ray 1,
>   _tax  = Ray 1,
>   _hat  = Wad 0,
>   _lag  = Sec 0,
>   _chi  = Ray 1,
>   _rum  = Wad 0,
>   _rho  = Sec 0
> }

> defaultUrn :: Id Ilk -> Address -> Urn
> defaultUrn id_ilk id_lad = Urn {
>   _vow  = Nothing,
>   _cat  = Nothing,
>   _lad  = id_lad,
>   _ilk  = id_ilk,
>   _art  = Wad 0,
>   _jam  = Wad 0
> }

> initialVat :: Ray -> Vat
> initialVat how0 = Vat {
>   _tau   = 0,
>   _fix   = Wad 1,
>   _par   = Wad 1,
>   _how   = how0,
>   _way   = Ray 1,
>   _joy   = Wad 0,
>   _sin   = Wad 0,
>   _ilks  = empty,
>   _urns  = empty,
>   _jars  =
>     singleton id_dai Jar {
>       _gem   = Gem {
>         _balanceOf    = empty
>       },
>       _tag  = Wad 0,
>       _zzz  = 0
>     }
> }

> initialSystem :: Ray -> System
> initialSystem how0 = System {
>   _vat       = initialVat how0,
>   _era       = 0,
>   _sender    = id_god,
>   _accounts  = mempty
> }

\chapter{Acts}

The \emph{acts} are the basic state transitions of the system.

For details on the underlying ``Maker monad,'' which specifies how the
act definitions behave with regard to state and rollback thereof, see
chapter~\ref{chapter:monad}.

\newpage
\section{Assessment}

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
>     min  = con * cast (view mat ilk0)
>

\clearpage

\newcommand{\yep}{$\bullet$}
\begin{table}[t]
\caption{|cdp| acts in the five stages of risk}\label{table:stages}
\vspace{0.25cm}
\resizebox{\textwidth}{!}{%
\begin{tabular}{ r c c c c c c c c c l }
&|give|&|shut|&|lock|&|wipe|&|free|&|draw|&|bite|&|grab|&|plop|& \\
|Pride|&\yep&\yep&\yep&\yep&\yep&\yep&&&& overcollateralized \\
|Anger|&\yep&\yep&\yep&\yep&\yep&&&&& debt ceiling reached \\
|Worry|&\yep&\yep&\yep&\yep&&&&&& price feed in limbo \\
|Panic|&\yep&\yep&\yep&\yep&&&\yep&&& undercollateralized \\
|Grief|&\yep&&&&&&&\yep&& liquidation initiated \\
|Dread|&\yep&&&&&&&&\yep& liquidation in progress \\
\multicolumn{2}{c}{} &
\multicolumn{3}{c}{risk decreasing} &
\multicolumn{2}{c}{risk increasing} &
\multicolumn{3}{c}{risk unwinding} &
\end{tabular}}
\end{table}

Now we define the internal act |gaze| which returns the value of
|analyze| after ensuring the system state is updated.

> gaze id_urn = do
> -- Adjust target price and target rate
>   prod
>
> -- Update price of specific debt unit
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   drip id_ilk
>
> -- Read parameters for risk analysis
>   era0    <- use era
>   par0    <- use (vat . par)
>   urn0    <- look (vat . urns . ix id_urn)
>   ilk0    <- look (vat . ilks . ix (view ilk urn0  ))
>   jar0    <- look (vat . jars . ix (view jar ilk0  ))
>
> -- Return risk stage of |cdp|
>   return (analyze era0 par0 urn0 ilk0 jar0)

\clearpage
\section{Lending}

\actentry{|open|}{create |cdp|}
Any Ethereum address can open one or more accounts with the system
using |open|, specifying an account identifier (self-chosen) and a
|cdp| type.

> open id_urn id_ilk = do
>
> -- Fail if account identifier is taken
>   none (vat . urns . ix id_urn)
>
> -- Create a |cdp| record with the sender as owner
>   id_lad <- use sender
>   initialize (vat . urns . at id_urn)
>     (defaultUrn id_ilk id_lad)
>     

\actentry{|give|}{transfer |cdp| account} The owner of a |cdp| can
transfer its ownership at any time using |give|.

> give id_urn id_lad = do
>
> -- Fail if sender is not the |cdp| owner
>   id_sender <- use sender
>   owns id_urn id_sender
>
> -- Transfer ownership
>   vat . urns . ix id_urn . lad .= id_lad

\actentry{|lock|}{deposit collateral}Unless liquidation has begun for
a |cdp|, its owner can use |lock| to deposit more collateral, thus
increasing the collateralization ratio.

> lock id_urn wad_gem = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Fail if liquiditation initiated
>   want (gaze id_urn) (< Grief)
>
> -- Identify collateral type
>   id_ilk  <- look (vat . urns . ix id_urn  . ilk)
>   id_jar  <- look (vat . ilks . ix id_ilk  . jar)
>
> -- Transfer tokens from owner to collateral vault
>   id_lad  <- use sender
>   pull id_jar id_lad wad_gem
>
> -- Record an increase in collateral
>   increase (vat . urns . ix id_urn . jam) wad_gem

\actentry{|free|}{withdraw collateral}When a |cdp| is
overcollateralized, its owner can use |free| to withdraw any amount of
collateral, as long as the withdrawal would not
cause undercollateralization.

> free id_urn wad_gem = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Record a decrease in collateral
>   decrease (vat . urns . ix id_urn . jam) wad_gem
>
> -- Roll back if withdrawal caused undercollateralization
>   want (gaze id_urn) (`elem` [Pride, Worry])
>
> -- Transfer tokens from collateral vault to owner
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   id_jar  <- look (vat . ilks . ix id_ilk . jar)
>   push id_jar id_lad wad_gem

\actentry{|draw|}{issue dai as debt}A |cdp| owner can use |draw| to
issue and lend an amount of dai, thus increasing the |cdp|'s debt and
lowering its collateralization ratio---as long as the |cdp| type's
debt ceiling is not reached and the loan would not result
in undercollateralization.

> draw id_urn wad_dai = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Update value of |cdp| type's debt unit
>   id_ilk     <- look (vat . urns . ix id_urn . ilk)
>   chi1       <- drip id_ilk
>
> -- Denominate draw amount in debt unit
>   let  wad_chi = wad_dai / cast chi1
>
> -- Increase |cdp| debt
>   increase (vat . urns . ix id_urn . art) wad_chi
>
> -- Increase total debt of |cdp| type
>   increase (vat . ilks . ix id_ilk . rum) wad_chi
>
> -- Roll back if loan caused undercollateralization or debt excess
>   want (gaze id_urn) (== Pride)
>
> -- Mint dai and send to |cdp| owner
>   mint id_dai wad_dai
>   push id_dai id_lad wad_dai

\actentry{|wipe|}{repay debt and burn dai}A |cdp| owner who has
previously loaned dai can use |wipe| to repay part of their debt as
long as liquidation has not been initiated.

> wipe id_urn wad_dai = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Fail if liquidation initiated
>   want (gaze id_urn) (< Grief)
>
> -- Update value of debt unit
>   id_ilk <- look (vat . urns . ix id_urn . ilk)
>   chi1   <- drip id_ilk
>
> -- Denominate dai amount in debt unit
>   let  wad_chi = wad_dai / cast chi1
>
> -- Decrease |cdp| debt
>   decrease (vat . urns . ix id_urn . art) wad_chi
>
> -- Decrease total |cdp| type debt
>   decrease (vat . ilks . ix id_ilk . rum) wad_chi
>
> -- Transfer dai from |cdp| owner to dai vault
>   pull id_dai id_lad wad_dai
>
> -- Destroy reclaimed dai
>   burn id_dai wad_dai

\actentry{|shut|}{wipe, free, and delete |cdp|}A |cdp| owner can use
|shut| to close their account---repaying all debt and reclaiming all
collateral---if the price feed is up to date and liquidation has not
been initiated.

> shut id_urn = do
>
>   -- Update value of debt unit
>     id_ilk <- look (vat . urns . ix id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Reclaim all outstanding dai
>     art0 <- look (vat . urns . ix id_urn . art)
>     wipe id_urn (art0 * cast chi1)
>
>   -- Reclaim all collateral
>     jam0 <- look (vat . urns . ix id_urn . jam)
>     free id_urn jam0
>
>   -- Nullify |cdp| record
>     vat . urns . at id_urn .= Nothing

\clearpage
\section{Adjustment}

\actentry{|prod|}{adjust target price and target rate}

> prod = do
>
>   era0  <- use era
>   tau0  <- use (vat . tau)
>   fix0  <- use (vat . fix)
>   par0  <- use (vat . par)
>   how0  <- use (vat . how)
>   way0  <- use (vat . way)
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
>   rho0  <- look (vat . ilks . ix id_ilk . rho)  -- Time stamp of previous |drip|
>   tax0  <- look (vat . ilks . ix id_ilk . tax)  -- Current stability fee
>   chi0  <- look (vat . ilks . ix id_ilk . chi)  -- Current value of debt unit
>   rum0  <- look (vat . ilks . ix id_ilk . rum)  -- Current total debt in debt unit
>   joy0  <- look (vat . joy)                     -- Current unprocessed stability fee revenue
>   era0  <- use era                              -- Current time stamp
>
>   let
>     age   = era0 - rho0
>     chi1  = chi0 * tax0 ^^ age
>     joy1  = joy0 + (cast (chi1 - chi0) :: Wad) * rum0
>
>   vat . ilks . ix id_ilk . chi  .= chi1
>   vat . ilks . ix id_ilk . rho  .= era0
>   vat . joy  .= joy1
>
>   return chi1

\section{Feedback}

\actentry{|mark|}{update market price of dai}

> mark id_jar tag1 zzz1 =
>   auth $ do
>     vat . jars . ix id_jar . tag  .= tag1
>     vat . jars . ix id_jar . zzz  .= zzz1

\actentry{|tell|}{update market price of collateral token}

> tell wad_gem =
>   auth $ do
>     vat . fix .= wad_gem

\section{Liquidation}

\actentry{|bite|}{mark for liquidation}

> bite id_urn = do
>
>   -- Fail if |cdp| is not in need of liquidation
>     want (gaze id_urn) (== Panic)
>
>   -- Record the sender as the liquidation initiator
>     id_cat              <- use sender
>     vat . urns . ix id_urn . cat  .= Just id_cat
>
>   -- Read current debt
>     art0    <- look (vat . urns . ix id_urn . art)
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
>     let art1 = art0 * cast axe0
>
>   -- Update |cdp| debt
>     vat . urns . ix id_urn . art   .=  art1
>
>   -- Record as bad debt
>     increase (vat . sin) (art1 * cast chi1)

\actentry{|grab|}{take tokens for liquidation}

> grab id_urn =
>
>   auth $ do
>
>   -- Fail if |cdp| is not marked for liquidation
>     want (gaze id_urn) (== Grief)
>
>   -- Record the sender as the |cdp|'s settler
>     id_vow <- use sender
>     vat . urns . ix id_urn . vow .= Just id_vow
>
>   -- Forget the |cdp|'s requester of liquidation
>     vat . urns . ix id_urn . cat .= Nothing

\actentry{|plop|}{finish liquidation returning profit}

> plop id_urn wad_dai =
>   auth $ do
>
>   -- Fail unless |cdp| is in liquidation
>     want (gaze id_urn) (== Dread)
>
>   -- Forget the |cdp|'s settler
>     vat . urns . ix id_urn . vow .= Nothing
>
>   -- Return some amount of excess auction gains
>     vat . urns . ix id_urn . jam .= wad_dai

\actentry{|heal|}{process bad debt}

> heal wad_dai =
>   auth $ do
>     decrease (vat . sin) wad_dai

\actentry{|love|}{process stability fee revenue}

> love wad_dai =
>   auth $ do
>     decrease (vat . joy) wad_dai

\section{Governance}

\actentry{|form|}{create a new |cdp| type}

> form id_ilk id_jar =
>   auth $ do
>     initialize (vat . ilks . at id_ilk)
>        (defaultIlk id_jar)

\actentry{|frob|}{set the sensitivity parameter}

> frob how1 =
>   auth $ do
>     vat . how .= how1

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

\section{Treasury}

\actentry{|pull|}{take tokens to vault}

> pull id_jar id_lad wad_gem =
>
>   transfer id_jar wad_gem
>     (InAccount  id_lad)
>     (InVault    id_jar)

\actentry{|push|}{send tokens from vault}

> push id_jar id_lad wad_gem =
>
>   transfer  id_jar wad_gem
>     (InVault    id_jar)
>     (InAccount  id_lad)
>

\actentry{|mint|}{create tokens}

> mint id_jar wad0 =
>   zoom (vat . jars . ix id_jar . gem) $ do
>     increase (balanceOf . ix (InVault id_jar)) wad0

\actentry{|burn|}{destroy tokens}

> burn id_jar wad0 =
>   zoom (vat . jars . ix id_jar . gem) $ do
>     decrease (balanceOf . ix (InVault id_jar)) wad0

\section{Manipulation}

\actentry{|warp|}{travel through time}

> warp t = auth (do increase era t)

\actentry{|mine|}{create toy token type}

> mine id_jar = do
>
>     initialize (vat . jars . at id_jar)
>       (Jar {
>          _gem   = Gem (singleton InToy 1000000000000),
>          _tag  = Wad 0,
>          _zzz  = 0 })

\actentry{|hand|}{give toy tokens to account}

> hand dst wad_gem id_jar = do
>   transfer id_jar wad_gem
>     InToy (InAccount dst)

\actentry{|sire|}{register a new toy account}

> sire lad = do prepend accounts lad

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

> being :: Act -> Address -> Maker ()
> being x who = do
>   old     <- use sender
>   sender  .= who
>   y       <- perform x
>   sender  .= old
>   return y

> transfer id_jar wad src dst  =
>
> -- Operate in the token's balance table
>   zoom (vat . jars . ix id_jar . gem . balanceOf) $ do
>
>   -- Fail if source balance insufficient
>     balance <- look (ix src)
>     aver (balance >= wad)
>
>   -- Decrease source balance
>     decrease    (ix src)  wad
>
>   -- Increase destination balance
>     initialize  (at dst)  0
>     increase    (ix dst)  wad

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

> data Error = AssertError Act | AuthError
>   deriving (Show, Eq)

\section{The |Maker| monad}
\label{section:maker-monad}

The reader does not need any abstract understanding of monads to
understand the code.  What they give us is a nice syntax---the |do|
notation---for expressing exceptions and state in a way that is still
purely functional.

> newtype Maker' s a =
>   Maker (StateT s (Except Error) a)
>
>   deriving
>     (  Functor, Applicative, Monad,
>        MonadError   Error,
>        MonadState   s)

> type Maker a = Maker' System a

> type instance Zoomed (Maker' s) = Focusing (Except Error)
> instance Zoom (Maker' s) (Maker' t) s t where
>   zoom l (Maker m) = Maker (zoom l m)

> exec  ::  System
>       ->  Maker ()
>       ->  Either Error System
> exec sys (Maker m) =
>   runExcept (execStateT m sys)

\section{Asserting}

> aver x = unless x (throwError (AssertError ?act))

> none x = preuse x >>= \case
>   Nothing -> return ()
>   Just _  -> throwError (AssertError ?act)

> look f = preuse f >>= \case
>   Nothing -> throwError (AssertError ?act)
>   Just x  -> return x

> want m p = m >>= (aver . p)

We define |owns id_urn id_lad| as an assertion that the given |cdp| is
owned by the given account.

> owns id_urn id_lad = do
> 
>   want (look (vat . urns . ix id_urn . lad)) (== id_lad)


\section{Modifiers}

\actentry{|auth|}{authenticating actions}

> auth continue = do
>   s <- use sender
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

%include Maker/Prelude.lhs
%include Maker/Decimal.lhs

\end{document}
