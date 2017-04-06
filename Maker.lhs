%include preamble.fmt

\chapter{Introduction}

The \textsc{dai credit system}, henceforth also ``Maker,'' is a
network of Ethereum contracts designed to issue the |dai| currency
token and automatically adjust credit incentives in order to keep its
market value stable relative to |sdr|\footnote{``Special Drawing
Rights'' (ticker symbol |xdr|), the international reserve asset
created by the International Monetary Fund, whose value is derived
from a weighted basket of world currencies.} in the short and
medium term.

New dai enters the money supply when a borrower locks an excess of
collateral in the system and takes out a loan.  The debt and
collateral amounts are recorded in a \textit{collateralized debt
position}, or |cdp|.  Thus all outstanding dai represents some |cdp|
owner's claim on their collateral---until risk provokes a liquidation.

Off-chain \textit{price feeds} give Maker knowledge of the market
values of dai and the various tokens used as collateral, enabling the
system to assess credit risk. If the value of a |cdp|'s collateral
drops below a certain multiple of its debt, a decentralized auction is
triggered which liquidates the collateral for dai in order to settle
the debt.

The system issues a separate token with symbol |mkr|.
Since collateral auctions may fail to recover the full value of
liquidated debt, the |mkr| token can be diluted to back emergency
debt. The value of |mkr|, though volatile by design, is backed by the
revenue from \textit{stability fees} imposed on all dai loans. The
|dai| raised from stability fees is used to buy |mkr| tokens from the
market and destroy them.

For more details on the economics of the system, as well as
descriptions of governance, off-chain mechanisms that provide
efficiency, and so on, see the white\-paper.

This document is an executable technical specification of the of the
Maker smart contracts.  It is a draft; be aware that the contents
will certainly change before launch.

\section{Naming}

The implementation is formulated in terms of a parallel vocabulary
whose concise words can seem meaningless at first glance (e.g., |Urn|,
|par|, |ink|).  These words are in fact carefully selected for
metaphoric resonance and evocative qualities.  Definitions of the
words along with mnemonic reminders can be found in the glossary.

We have found that though it requires some initial indoctrination, the
Maker jargon is good for development and helps when thinking and
talking about the structure and mechanics of the system.  Here are
some of the reasons:

\begin{itemize}

\item
The parallel jargon
 lets us sidestep
  terminological debates; for example, whether to say
  ``rate of target price change'' or
   ``target rate.''

\item
With decoupled
 financial and technical
  vocabularies,
 we can more flexibly
  improve one
   without affecting the other.

\item
The ability to discuss the system formally,
 with the financial interpretation partly suspended,
  has suggested insights that would have been
   harder to think of inside the normal language. 

\item
The precise and distinctive language
 makes the structure and logic
  of the implementation
   more apparent
    and easier to formalize.

\end{itemize}

Some readers may perceive the Maker terminology as unnecessarily
obscure despite our apologetics.  In that case, we recommend a
contrasting look at the Ethereum ``yellow paper,'' after which this
document should appear highly legible.

\section{Motivation}

The version of this system that will be deployed on the blockchain is
written in Solidity, which is a workable smart contract implementation
language.  This reference implementation is a model of the behavior of
those contracts, written as a ``literate'' Haskell program.
The motivations for such a reference implementation include:

\begin{enumerate}

\item \textbf{Comparison.}  Checking two free-standing implementations
against each other is a well-known way of ensuring that they both
behave as intended.

\item \textbf{Testing.}  Haskell lets us use powerful testing tools
such as QuickCheck and SmallCheck for comprehensively verifying key
properties as a middle ground between unit testing and
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

\item \textbf{Clarity.}  An implementation not intended to be deployed
on the blockchain is free from concerns about optimizing for gas cost
and other factors that make the Solidity implementation less ideal as
an understandable specification.

\item \textbf{Simulation.}  Solidity is highly specific to the
Ethereum blockchain environment and as such does not have facilities
for interfacing with files or other computer programs.  This makes the
Solidity implementation of the system less useful for doing
simulations of the system's economic, game-theoretic, or
statistical aspects.

\end{enumerate}

\section{Limitations}

This model is limited in that it has

\begin{enumerate}
\item a simplified version of authorization for governance;
\item a simplified version of |ERC20| token semantics;
\item no implementation of the decentralized auction contracts; and
\item no 256-bit word limits.
\end{enumerate}

These limitations will be addressed in future revisions.

\section{Verification}

Separately from this document, we are developing automatic test suites
that generate many, large, and diverse action sequences for property
verification.  One such property is that the reference implementation
exactly matches the on-chain implementation; this is verified through
the generation of Solidity test cases with assertions covering the
entire state.  Other key properties include

\begin{itemize}
\item that the target price changes only according to the target rate;
\item that the total dai supply is fully accounted for by |cdp| debts;
\item that |cdp| acts are restricted with respect to risk stage;
\end{itemize}

along with similar invariants and conditions.  A future revision of
this document will include formal statements of these properties.

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

This is a Haskell program, and as such makes reference to a background
of symbols defined in libraries, as a mathematical paper depends on
preestablished theories.

Context should allow the reader to understand most symbols without
further reading, but Appendix~\ref{appendix:prelude} lists and briefly
explains each imported type and function.

We replace the default prelude module with our own.

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
Now we proceed to define the specifics of the Maker system.

\chapter{Types}

This chapter defines the data types used by Maker: numeric types,
identifiers, on-chain records, and test model data.

Haskell syntax note: |newtype| defines a type synonym with distinct
type identity; |data| creates a record type; and |deriving| creates
automatic instances of common functionality.

\section{Numeric types}

The system uses two different precisions of decimal fixed point
numbers, which we call \emph{wads} and \emph{rays}, having
respectively 18 digits of precision (used for token quantities) and 36
digits (used for precise rates and ratios).
See Appendix~\ref{appendix:numbers} for details on decimal fixed point
numbers and rounding.

> -- Define the distinct type of currency quantities
> newtype Wad = Wad (Decimal E18)
>   deriving (  Ord, Eq, Num, Real, Fractional, RealFrac)
>
> -- Define the distinct type of rates and ratios
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
We also define a type for time durations in whole seconds, as this is
the maximum precision allowed by the Ethereum virtual machine.

> newtype Sec = Sec Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)

Haskell number types are not automatically converted, so we convert
explicitly with a |cast| function.

> -- Convert via fractional $n/m$ form.
> cast :: (Real a, Fractional b) => a -> b
> cast = fromRational . toRational


\section{Identifiers and addresses}

There are several kinds of identifiers used in the system, and we use
types to distinguish them.  The type parameter |a| creates distinct
types; e.g., |Id Foo| and |Id Bar| are incompatible.

> newtype Id a = Id String deriving (Eq, Ord, Show)

We define another type for representing Ethereum account addresses.

> newtype Address = Address String deriving (Eq, Ord, Show)

We also have predefined entity identifiers.

> -- The |dai| token identifier
> id_dai = Id "DAI"
>
> -- The internal debt token identifier
> id_sin = Id "SIN"
>
> -- The |mkr| token identifier
> id_mkr = Id "MKR"
>
> -- A test account with ultimate authority
> id_god = Address "GOD"

%if 0

> instance Read (Id a) where
>   readsPrec n s = fmap (first Id) (readsPrec n s)

%endif

\section{|Jar| --- collateral vault}
\actentry{|gem|}{collateral token}
\actentry{|tag|}{market price of token}
\actentry{|zzz|}{expiration time of token price feed}

The data received from price feeds is categorized by token and stored
in |Jar| records.  Our model also has the token balances embedded in
these records; in reality\footnote{We use ``reality'' to denote the
actual state of the consensus Ethereum blockchain.}, the balances are
in separate |ERC20| contracts.

> data Jar = Jar {
>
>     _gem  :: Gem,  -- Token balances
>     _tag  :: Wad,  -- Market price denominated in |SDR|
>     _zzz  :: Sec   -- Time of price expiration
>
>   } deriving (Eq, Show)

\section{|Gem| --- token model}

In reality, token semantics can differ, despite nominally following
the |erc20| interface.  Governance therefore involves reviewing the
behaviors of collateral tokens.  In our model, tokens behave in the
same simple way.  We also omit the notion of ``allowance.''

We define a |Gem| record as a map tracking the token quantity held
by each holding entity.

> data Gem = Gem { _balanceOf :: Map Entity Wad }
>   deriving (Eq, Show)

For clarity, we use a data type to distinguish the different entities
that can hold a token balance.  

> data Entity  =  Account Address  -- External account or contract
>              |  Vault (Id Jar)   -- Vault for collateral token
>              |  Joy              -- Spawning account for dai
>              |  Woe              -- Spawning account for debt
>              |  Ice              -- Holding account for debt
>              |  Vow              -- Settler
>              |  Toy              -- Test driver
>   deriving (Eq, Ord, Show)

\section{|Ilk| --- |cdp| type}
\actentry{|jar|}{collateral token vault}
\actentry{|mat|}{liquidation ratio}
\actentry{|axe|}{liquidation penalty}
\actentry{|hat|}{debt ceiling}
\actentry{|tax|}{stability fee}
\actentry{|lax|}{price feed limbo duration}
\actentry{|rho|}{time of debt unit adjustment}
\actentry{|rum|}{total outstanding debt units}
\actentry{|chi|}{value of debt unit in |dai|}

Each |cdp| belongs to a |cdp| type, specified by an |Ilk| record.
Five parameters, |mat|, |axe|, |hat|, |tax| and |lax|, are set by
governance and are known as the \emph{risk parameters}. The rest of
the values are used by the system to keep track of the current state.
The meaning of each |ilk| parameter is defined by its interactions in
the act definitions of Chapter~\ref{chapter:acts}; see the whitepaper
for an overview.

> data Ilk = Ilk {
>
>     _jar  :: Id Jar,  -- Collateral token identifier
> 
>     _lax  :: Sec,     -- Grace period after price feed becomes unavailable
>     _mat  :: Ray,     -- Collateral-to-debt ratio at which liquidation can be triggered
>     _axe  :: Ray,     -- Penalty on liquidation as fraction of debt
>     _hat  :: Wad,     -- Limit on total dai debt for |cdp| type (``debt ceiling'')
>     _tax  :: Ray,     -- Stability fee as per-second fraction of debt value
> 
>     _chi  :: Ray,     -- Value of internal debt unit in dai
>     _rho  :: Sec,     -- Time of latest debt unit adjustment
>     _rum  :: Wad      -- Total debt in debt units
>
>   } deriving (Eq, Show)

\section{|Urn| --- collateralized debt position (|cdp|)}
\actentry{|cat|}{address of liquidation initiator}
\actentry{|lad|}{|cdp| owner}
\actentry{|ilk|}{|cdp| type}
\actentry{|art|}{debt denominated in debt unit}
\actentry{|ink|}{collateral denominated in debt unit}
For each |cdp| we maintain an |Urn| record identifying its type and
specifying ownership, quantities of debt and collateral denominated in
the |cdp| type's debt unit, along with who triggered liquidation (if
applicable).

> data Urn = Urn {
>
>     _ilk  :: Id Ilk,         -- Identifier of |cdp| type
>     _lad  :: Entity,         -- Owner of |cdp|
> 
>     _art  :: Wad,            -- Outstanding debt in debt unit
>     _ink  :: Wad,            -- Collateral amount in debt unit
> 
>     _cat  :: Maybe Entity    -- Entity that triggered liquidation, if applicable
>
>   } deriving (Eq, Show)

\section{|Vox| --- feedback mechanism data}
\actentry{|wut|}{market price of |dai| denominated in |sdr|}
\actentry{|par|}{target price of |dai| denominated in |sdr|}
\actentry{|how|}{sensitivity parameter}
\actentry{|way|}{rate of target price change}
\actentry{|tau|}{time of latest feedback cycle}

The \emph{feedback mechanism} is the aspect of the |cdp| engine that
adjusts the target price of dai based on market price, and its data is
kept in a singleton record called |Vox|.

> data Vox = Vox {
> 
>     _wut   :: Wad,    -- Market price of dai denominated in |sdr|
>     _par   :: Wad,    -- Target price of dai denominated in |sdr|
>     _way   :: Ray,    -- Current per-second change in target price
> 
>     _how   :: Ray,    -- Sensitivity parameter set by governance
>     _tau   :: Sec     -- Time of latest feedback cycle
> 
>  } deriving (Eq, Show)

Keeping the feedback data separate allows us to more easily upgrade
the mechanism in the future.

\section{|Vat| --- |cdp| engine aggregate}

The |Vat| record aggregates the records of tokens, |cdp|s, |cdp|
types, and price feeds, along with the data of the feedback mechanism.

> data Vat = Vat {
> 
>     _jars  :: Map (Id Jar) Jar,   -- Collateral vaults
>     _ilks  :: Map (Id Ilk) Ilk,   -- |cdp| type records
>     _urns  :: Map (Id Urn) Urn,   -- |cdp| records
> 
>     _vox   :: Vox                 -- Data of feedback mechanism
> 
>   } deriving (Eq, Show)

\section{System model}
\actentry{|era|}{current time}

Finally we define a record with no direct counterpart in the Solidity
contracts, which has the |Vat| record along with model state.

> data System =  System {
>
>     _vat       :: Vat,        -- Root Maker entity
>     _era       :: Sec,        -- Current time stamp
>     _sender    :: Entity,     -- Sender of current act
>     _accounts  :: [Address],  -- For test suites
>     _mode      :: Mode        -- Vow operation mode
>
>   } deriving (Eq, Show)

> data Mode = Dummy
>   deriving (Eq, Show)

%if 0

\section*{Lens fields}

> makeLenses ''Gem  ; makeLenses ''Jar  ; makeLenses ''Ilk
> makeLenses ''Urn    ; makeLenses ''Vox  ; makeLenses ''Vat
> makeLenses ''System

> balance id_gem entity =
>   vat . jars . ix id_gem . gem . balanceOf . ix entity

%endif

\section{Default data}

> defaultIlk :: Id Jar -> Ilk
> defaultIlk id_jar = Ilk {
>   _jar  = id_jar,
>   _axe  = Ray 1,
>   _mat  = Ray 1,
>   _tax  = Ray 1,
>   _hat  = Wad 0,
>   _lax  = Sec 0,
>   _chi  = Ray 1,
>   _rum  = Wad 0,
>   _rho  = Sec 0
> }

> emptyUrn :: Id Ilk -> Entity -> Urn
> emptyUrn id_ilk id_lad = Urn {
>   _cat  = Nothing,
>   _lad  = id_lad,
>   _ilk  = id_ilk,
>   _art  = Wad 0,
>   _ink  = Wad 0
> }

> initialJar :: Id Jar -> Jar
> initialJar id_jar = Jar {
>   _gem  = Gem { _balanceOf = singleton (Vault id_jar) 0 },
>   _tag  = Wad 0,
>   _zzz  = 0
> }

> initialVat :: Ray -> Vat
> initialVat how0 = Vat {
>   _vox   = Vox {
>     _tau   = 0,
>     _wut   = Wad 1,
>     _par   = Wad 1,
>     _how   = how0,
>     _way   = Ray 1
>   },
>   _ilks  = empty,
>   _urns  = empty,
>   _jars  = fromList [
>     (id_dai, initialJar id_dai),
>     (id_sin, initialJar id_sin),
>     (id_mkr, initialJar id_mkr)
>   ]
> }

> initialSystem :: Ray -> System
> initialSystem how0 = System {
>   _vat       = initialVat how0,
>   _era       = 0,
>   _sender    = Account id_god,
>   _accounts  = mempty
> }

\chapter{Acts}
\label{chapter:acts}

The \emph{acts} are the basic state transitions of the system.

Unless specified as \emph{internal}, acts are accessible as public
functions on the blockchain.

The |auth| modifier marks acts which can only be invoked from
addresses to which the system has granted authority.

For details on the underlying ``Maker monad,'' which specifies how the
act definitions behave with regard to state and rollback, see
chapter~\ref{chapter:monad}.

\newpage
\section{Assessment}

\actentry{|feel|}{identify |cdp| risk stage}

In order to prohibit |cdp| acts based on risk situation, we define
five stages of risk.

> data Stage  =  Pride |  Anger |  Worry |  Panic |  Grief |  Dread
> 
>   deriving (Eq, Show)

We define the function |analyze| that determines the risk stage of a
|cdp|.

> analyze era0 par0 urn0 ilk0 jar0 =
>   if  | view cat  urn0  /= Nothing && view ink urn0 == 0
>         -- |cdp| liquidation triggered and started
>          -> Dread
>       | view cat  urn0  /= Nothing
>         -- |cdp| liquidation triggered
>          -> Grief  
>       | pro < min
>         -- |cdp|'s collateralization below liquidation ratio
>          -> Panic  
>       | view zzz jar0 + view lax ilk0 < era0
>         -- |cdp| type's price limbo exceeded limit
>          -> Panic  
>       | view zzz jar0 < era0
>         -- |cdp| type's price feed in limbo
>          -> Worry  
>       | cap  > view hat ilk0
>         -- |cdp| type's debt ceiling exceeded
>          -> Anger
>       | otherwise   
>         -- No problems
>          -> Pride
>
>   where
>   -- |cdp|'s collateral value in |sdr|:
>     pro  = view ink urn0  * view tag jar0
>
>   -- |cdp| type's total debt in |dai|:
>     cap  = view rum ilk0  * cast (view chi ilk0)
>
>   -- |cdp|'s debt in |sdr|:
>     con  = view art urn0  * cast (view chi ilk0) * par0
>
>   -- Required collateral as per liquidation ratio:
>     min  = con * cast (view mat ilk0)
>

\clearpage

\newcommand{\yah}{\faHandPeaceO}
\newcommand{\yep}{\faHandScissorsO}
\newcommand{\hey}{\faHandGrabO}
\newcommand{\meh}{\faHandPointerO}
\newcommand{\woo}{\faHandPaperO}
\newcommand{\nah}{---}
\begin{table}[t]
\caption{|cdp| acts in the five stages of risk}\label{table:stages}
\vspace{0.25cm}
%\resizebox{\textwidth}{!}{%
\begin{tabular}{ r c c c c c c c c c }
       &|give|&|shut|&|lock|&|wipe|&|free|&|draw|&|bite|&|grab|&|plop| \\
|Pride|&\yah  &\yep  &\yep  &\yep  &\meh  &\meh  &\nah  &\nah  &\nah \\
|Anger|&\yah  &\yep  &\yep  &\yep  &\meh  &\nah  &\nah  &\nah  &\nah \\
|Worry|&\yah  &\yep  &\yep  &\yep  &\nah  &\nah  &\nah  &\nah  &\nah \\
|Panic|&\yah  &\yep  &\yep  &\yep  &\nah  &\nah  &\woo  &\nah  &\nah \\
|Grief|&\yah  &\nah  &\nah  &\nah  &\nah  &\nah  &\nah  &\hey  &\nah \\
|Dread|&\yah  &\nah  &\nah  &\nah  &\nah  &\nah  &\nah  &\nah  &\hey \\
&  &
\multicolumn{3}{c}{decrease risk} &
\multicolumn{2}{c}{increase risk} &
\multicolumn{3}{c}{unwind risk}
\end{tabular} %}
\vspace{0.5cm}
\caption*{
\begin{tabular} { c l }
\woo & allowed for anyone \\
\yah & allowed for owner unconditionally \\
\yep & allowed for owner if able to repay \\
\meh & allowed for owner if collateralized \\
\hey & allowed for settler contract \\
\end{tabular}
}
\end{table}

Now we define the internal act |feel| which returns the value of
|analyze| after ensuring that the system state is updated.

> feel id_urn = do
> -- Adjust target price and target rate
>   prod
>
> -- Update debt unit and unprocessed fee revenue
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   drip id_ilk
>
> -- Read parameters for risk analysis
>   era0    <- use   era
>   par0    <- use   (vat . vox . par)
>   urn0    <- look  (vat . urns . ix id_urn)
>   ilk0    <- look  (vat . ilks . ix (view ilk urn0  ))
>   jar0    <- look  (vat . jars . ix (view jar ilk0  ))
>
> -- Return risk stage of |cdp|
>   return (analyze era0 par0 urn0 ilk0 jar0)

Acts on |cdp|s use |feel| to prohibit increasing risk when already
risky, and to freeze debt and collateral during liquidation; see
Table~\ref{table:stages}.

\clearpage
\section{Lending}

\actentry{|open|}{create |cdp|} Any user can open one or more accounts
with the system using |open|, specifying a self-chosen account
identifier and a |cdp| type.

> open id_urn id_ilk = do
>
> -- Fail if account identifier is taken
>   none (vat . urns . ix id_urn)
>
> -- Create a |cdp| record with the sender as owner
>   id_lad <- use sender
>   initialize (vat . urns . at id_urn) (emptyUrn id_ilk id_lad)

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

\actentry{|lock|}{deposit collateral}Unless liquidation has been
triggered for a |cdp|, its owner can use |lock| to deposit more
collateral.

> lock id_urn wad_gem = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Fail if liquidation triggered or initiated
>   want (feel id_urn) (`notElem` [Grief, Dread])
>
> -- Identify collateral type
>   id_ilk  <- look (vat . urns . ix id_urn  . ilk)
>   id_jar  <- look (vat . ilks . ix id_ilk  . jar)
>
> -- Transfer tokens from owner to collateral vault
>   pull id_jar id_lad wad_gem
>
> -- Record an increase in collateral
>   increase (vat . urns . ix id_urn . ink) wad_gem

\clearpage

\actentry{|free|}{withdraw collateral}When a |cdp| has no risk
problems (except that its |cdp| type's debt ceiling may be exceeded),
its owner can use |free| to withdraw some amount of collateral, as
long as the withdrawal would not reduce collateralization below the
liquidation ratio.

> free id_urn wad_gem = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Record a decrease in collateral
>   decrease (vat . urns . ix id_urn . ink) wad_gem
>
> -- Roll back on any risk problem except debt ceiling excess
>   want (feel id_urn) (`elem` [Pride, Anger])
>
> -- Transfer tokens from collateral vault to owner
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   id_jar  <- look (vat . ilks . ix id_ilk . jar)
>   push id_jar id_lad wad_gem

\actentry{|draw|}{issue dai as debt}When a |cdp| has no risk problems,
its owner can can use |draw| to take out a loan of newly minted dai,
as long as the |cdp| type's debt ceiling is not reached and the loan
would not result in undercollateralization.

> draw id_urn wad_dai = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Update debt unit and unprocessed fee revenue
>   id_ilk     <- look (vat . urns . ix id_urn . ilk)
>   chi1       <- drip id_ilk
>
> -- Denominate loan in debt unit
>   let  wad_chi = wad_dai / cast chi1
>
> -- Increase |cdp| debt
>   increase (vat . urns . ix id_urn . art) wad_chi
>
> -- Increase total debt of |cdp| type
>   increase (vat . ilks . ix id_ilk . rum) wad_chi
>
> -- Roll back on any risk problem
>   want (feel id_urn) (== Pride)
>
> -- Mint both dai and debt tokens
>   lend wad_dai
>
> -- Transfer dai to |cdp| owner
>   transfer id_dai wad_dai Joy id_lad
>
> -- Transfer sin into debt vault
>   transfer id_sin wad_dai Woe Ice 

\actentry{|wipe|}{repay debt and burn dai}A |cdp| owner who has
previously loaned dai can use |wipe| to repay part of their debt as
long as liquidation has not been triggered.

> wipe id_urn wad_dai = do
>
> -- Fail if sender is not the |cdp| owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Fail if liquidation triggered or initiated
>   want (feel id_urn) (`notElem` [Grief, Dread])
>
> -- Update debt unit and unprocessed fee revenue
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
> -- Destroy dai and corresponding debt tokens
>   mend wad_dai

\actentry{|shut|}{wipe, free, and delete |cdp|}A |cdp| owner can use
|shut| to close their account---repaying all debt and reclaiming all
collateral---if the price feed is up to date and liquidation has not
been initiated.

> shut id_urn = do
>
>   -- Update debt unit and unprocessed fee revenue
>     id_ilk <- look (vat . urns . ix id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Reclaim all outstanding dai
>     art0 <- look (vat . urns . ix id_urn . art)
>     wipe id_urn (art0 * cast chi1)
>
>   -- Reclaim all collateral
>     ink0 <- look (vat . urns . ix id_urn . ink)
>     free id_urn ink0
>
>   -- Nullify |cdp| record
>     vat . urns . at id_urn .= Nothing

\clearpage
\section{Adjustment}

\actentry{|prod|}{adjust target price and target rate}

The feedback mechanism is updated through |prod|, which can be invoked
at any time by keepers, but is also invoked as a side effect of any
|cdp| act that uses |feel| to assess the |cdp| risk.

> prod = do
>
> -- Read all parameters relevant for feedback mechanism
>   era0  <- use era
>   tau0  <- use (vat . vox . tau)
>   wut0  <- use (vat . vox . wut)
>   par0  <- use (vat . vox . par)
>   how0  <- use (vat . vox . how)
>   way0  <- use (vat . vox . way)
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
>                    if wut0 < par0 then wag else -wag)
>
> -- Update target price
>   vat.vox.par  .= par1
> -- Update rate of price change
>   vat.vox.way  .= way1
> -- Record time of update
>   vat.vox.tau  .= era0
>
>   where
>
>   -- Convert between multiplicative and additive form
>     prj x  = if x >= 1  then x - 1  else 1 - 1 / x
>     inj x  = if x >= 0  then x + 1  else 1 / (1 - x)



\clearpage
\actentry{|drip|}{update debt unit and unprocessed fee revenue}

The stability fee of a |cdp| type can change through governance.
Due to the constraint that acts should run in constant time, the
system cannot iterate over |cdp| records to effect such changes.
Instead each |cdp| type has a single ``debt unit'' which accumulates
the stability fee.  The |drip| act updates this unit.  It can be
called at any time by keepers, but is also called as a side effect of
every act that uses |feel| to assess |cdp| risk.

> drip id_ilk = do
>
> -- Time stamp of previous |drip|
>   rho0  <- look (vat . ilks . ix id_ilk . rho)
> -- Current stability fee
>   tax0  <- look (vat . ilks . ix id_ilk . tax)
> -- Current debt unit value
>   chi0  <- look (vat . ilks . ix id_ilk . chi)
> -- Current total debt in debt unit
>   rum0  <- look (vat . ilks . ix id_ilk . rum)
> -- Current time stamp
>   era0  <- use era
>
>   let
>   -- Time difference in seconds
>     age   = era0 - rho0
>   -- Value of debt unit increased according to stability fee
>     chi1  = chi0 * tax0 ^^ age
>   -- Stability fee revenue denominated in new unit
>     dew   = (cast (chi1 - chi0) :: Wad) * rum0
>
> -- Mint dai and internal debt tokens for marginal stability fee
>   lend dew
>
> -- Record time of update
>   vat . ilks . ix id_ilk . rho  .= era0
> -- Record new debt unit
>   vat . ilks . ix id_ilk . chi  .= chi1
>
> -- Return the new debt unit
>   return chi1

\clearpage
\section{Price feed input}

\actentry{|mark|}{update market price of collateral token}The |mark|
act records a new market price of a collateral token along with the
expiration date of this price.

> mark id_jar tag1 zzz1 = auth $ do
>     vat . jars . ix id_jar . tag  .= tag1
>     vat . jars . ix id_jar . zzz  .= zzz1

\actentry{|tell|}{update market price of dai}The |tell| act records a
new market price of the |dai| token along with the expiration date of
this price.

> tell wad_gem = auth $ do vat . vox . wut .= wad_gem

\section{Liquidation}

\actentry{|bite|}{mark for liquidation}%
When a |cdp|'s risk stage marks it
  as in need of liquidation,
 any account can invoke the |bite| act
  to trigger the liquidation process.
This enables the settler contract
 to grab the collateral for auctioning
 and take over the debt tokens
  representing ``bad debt.''

> bite id_urn = do
>
>   -- Fail if |cdp| is not in the appropriate risk stage
>     want (feel id_urn) (== Panic)
>
>   -- Record the sender as the liquidation initiator
>     id_cat     <- use sender
>     vat . urns . ix id_urn . cat  .= Just id_cat
>
>   -- Apply liquidation penalty to debt
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     axe0    <- look (vat . ilks . ix id_ilk . axe)
>     art0    <- look (vat . urns . ix id_urn . art)
>     let art1 = art0 * cast axe0
> 
>   -- Update debt
>     vat . urns . ix id_urn . art   .=  art1

\clearpage

\actentry{|grab|}{take tokens for liquidation}%
After liquidation has been triggered,
the designated settler contract invokes |grab|
to receive both the |cdp|'s collateral tokens
and the internal debt tokens
corresponding to the |cdp|'s debt.

> grab id_urn = auth $ do
>
>   -- Fail if |cdp| is not marked for liquidation
>     want (feel id_urn) (== Grief)
>
>     ink0    <- look (vat . urns . ix id_urn . ink)
>     art0    <- look (vat . urns . ix id_urn . art)
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     id_jar  <- look (vat . ilks . ix id_ilk . jar)
> 
>   -- Update the debt unit and stability fee
>     chi1    <- drip id_ilk
>
>   -- Denominate the debt in dai
>     let con = art0 * cast chi1
>
>   -- Transfer debt to settler
>     push id_sin Vow con
>
>   -- Transfer collateral to settler
>     push id_jar Vow ink0
> 
>   -- Nullify |cdp|'s collateral and debt quantities
>     vat . urns . ix id_urn . ink .= 0
>     vat . urns . ix id_urn . art .= 0
>
>   -- Decrease the |cdp| type's total debt quantity
>     decrease (vat . ilks . ix id_ilk . rum) art0

\actentry{|plop|}{finish liquidation returning profit}When the settler
has finished the process of liquidating a |cdp|'s collateral, it
invokes |plop| on the |cdp| to give back any excess collateral gains.

> plop id_urn wad_dai = auth $ do
>
>   -- Fail unless |cdp| is in liquidation
>     want (feel id_urn) (== Dread)
>
>   -- Forget the |cdp|'s requester of liquidation
>     vat . urns . ix id_urn . cat .= Nothing
>
>   -- Return some amount of excess auction gains
>     id_vow  <- use sender
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     id_jar  <- look (vat . ilks . ix id_ilk . jar)
>     pull id_jar id_vow wad_dai
>
>   -- Record the gains as the |cdp|'s collateral
>     vat . urns . ix id_urn . ink .= wad_dai

\clearpage

\actentry{|loot|}{take unprocessed stability fees}%
The settler can invoke |loot| at any time
to claim all uncollected stability fee revenue
(for use in the |mkr| buy and burn auction).

> loot = auth $ do
>
> -- The dai vault's balance is the uncollected stability fee revenue
>   wad_dai  <- look (balance id_dai (Vault id_dai))
>
> -- Transfer the entire dai vault balance to sender
>   id_vow   <- use sender
>   transfer id_dai wad_dai (Vault id_dai) Vow

\section{Auctioning}
\actentry{|flip|}{put collateral up for auction}%

> flip id_gem wad_jam wad_tab id_urn = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

\actentry{|flap|}{put fee revenue up for auction}%

> flap = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

\actentry{|flop|}{put |mkr| up for auction}%

> flop = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

\clearpage
\section{Settlement}
\actentry{|tidy|}{burn equal quantities of |dai| and |sin|}%


> tidy who = auth $ do
>
> -- Find the |dai| and |sin| balances of the entity
>   awe  <- look (balance id_dai  who)
>   woe  <- look (balance id_sin  who)
>
> -- We can burn at most the smallest of the two balances
>   let x = min awe woe
>
> -- Transfer both |dai| and |sin| into the vow accounts
>   transfer  id_dai  x who  Vow
>   transfer  id_sin  x who  Vow
>
> -- Burn both |dai| and |sin|
>   burn      id_dai  x      Vow
>   burn      id_sin  x      Vow

\actentry{|kick|}{flap, flop, and whatnot}

> kick = do
>
> -- Transfer unprocessed stability fee revenue to vow account
>   loot
> 
> -- Cancel fee revenue against bad debt; vow keeps either a |dai| balance \emph{or} a |sin| balance.
>   tidy Vow
>
> -- Assign any remaining revenue to the |mkr|-deflating fee auction
>   transferAll id_dai Vow Flapper
>   flap
>
> -- Assign any remaining debt to the |mkr|-inflating debt auction
>   transferAll id_sin Vow Flopper
>   flop

\section{Governance}

\actentry{|form|}{create a new |cdp| type}Governance uses |form| to
create a new |cdp| type.  Since the new type is initialized with a
zero debt ceiling, a separate transaction can safely set the risk
parameters before any lending occurs.

> form id_ilk id_jar = auth $ do
>     initialize (vat . ilks . at id_ilk) (defaultIlk id_jar)

\actentry{|frob|}{set the sensitivity parameter}Governance uses |frob|
to alter the sensitivity factor, which is the only mutable parameter
of the feedback mechanism.

> frob how1 = auth $ do vat . vox . how .= how1

\actentry{|chop|}{set liquidation penalty}\actentry{|cork|}{set debt ceiling}\actentry{|calm|}{set limbo duration}%
\actentry{|cuff|}{set liquidation ratio}%
Governance can alter the five risk parameters of a |cdp| type using
|cuff| for the liquidation ratio; |chop| for the liquidation penalty;
|cork| for the debt ceiling; |calm| for the duration of
price limbo; and |crop| for the stability fee.

> cuff id_ilk mat1  = auth $ do vat . ilks . ix id_ilk . mat  .= mat1
> chop id_ilk axe1  = auth $ do vat . ilks . ix id_ilk . axe  .= axe1
> cork id_ilk hat1  = auth $ do vat . ilks . ix id_ilk . hat  .= hat1
> calm id_ilk lax1  = auth $ do vat . ilks . ix id_ilk . lax  .= lax1

\actentry{|crop|}{set stability fee}%
When altering the stability fee with |crop|, we ensure that the
previous stability fee has been accounted for in the internal
debt unit.

> crop id_ilk tax1 =
>   auth $ do
>   -- Apply the current stability fee to the internal debt unit
>     drip id_ilk
>   -- Change the stability fee
>     vat . ilks . ix id_ilk . tax .= tax1

\section{Vaults}
\actentry{|pull|}{transfer tokens to vault}%
The internal act |pull| transfers tokens into a vault.
It is used by |lock| to acquire collateral from a |cdp| owner;
by |wipe| to acquire dai from a |cdp| owner;
and by |plop| to acquire collateral from the settler contract.

> pull id_jar src wad_gem =
>   transfer id_jar wad_gem src (Vault id_jar)

\actentry{|push|}{transfer tokens from vault}%
The internal act |push| transfers tokens out from a collateral vault.
It is used by |draw| to send dai to a |cdp| owner;
by |free| to send collateral to a |cdp| owner;
and by |grab| to send collateral to the settler contract.

> push id_jar dst wad_gem =
>   transfer id_jar wad_gem (Vault id_jar) dst

\section{Token manipulation}%
We model the |erc20| transfer function in simplified form (omitting
the concept of ``allowance'').

> transfer id_jar wad src dst  =
>
> -- Operate in the token's balance table
>   zoom (vat . jars . ix id_jar . gem . balanceOf) $ do
>
>   -- Fail if source balance insufficient
>     balance <- look (ix src)
>     aver (balance >= wad)
>
>   -- Update balances
>     decrease    (ix src)  wad
>     initialize  (at dst)  0
>     increase    (ix dst)  wad

\actentry{|mint|}{inflate token}%
The internal act |mint| inflates the supply of a token.
It is used by |lend| to create new |dai| and debt tokens,
and by the settler to create new |mkr|.

> mint id_jar wad0 dst =
>   zoom (vat . jars . ix id_jar . gem) $ do
>     increase (balanceOf . ix dst) wad0

\actentry{|burn|}{deflate token}%
The internal act |burn| deflates the supply of a token.
It is used by |mend| to destroy |dai| and debt tokens,
and by the settler to destroy |mkr|.

> burn id_jar wad0 src =
>   zoom (vat . jars . ix id_jar . gem) $ do
>     decrease (balanceOf . ix src) wad0

\actentry{|lend|}{mint dai and debt token}%
The internal act |lend| mints identical amounts
of both dai and the internal debt token.
It is used by |draw| to issue dai to a borrower;
it is also used by |drip| to issue dai
representing revenue from stability fees,
which stays in the dai vault until collected.

> lend wad_dai = do
>
>   mint id_dai wad_dai Joy
>   mint id_sin wad_dai Woe

\actentry{|mend|}{burn dai and debt token}%
The internal act |mend| destroys identical amounts
of both dai and the internal debt token.
Its use via |wipe| is how the dai supply is reduced.

> mend wad_dai = do
>
>   burn id_dai wad_dai (Vault id_dai)
>   burn id_sin wad_dai Ice

%if 0

\section{Manipulation}

\actentry{|warp|}{travel through time}

> warp t = auth (do increase era t)

\actentry{|mine|}{create toy token type}

> mine id_jar = do
>
>     initialize (vat . jars . at id_jar)
>       (Jar {
>          _gem  = Gem (singleton Toy 1000000000000),
>          _tag  = Wad 0,
>          _zzz  = 0 })

\actentry{|hand|}{give toy tokens to account}

> hand dst wad_gem id_jar = do
>   transfer id_jar wad_gem
>     Toy (Account dst)

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
>     Draw lad wad     -> draw lad wad
>     Cork urn wad     -> cork urn wad

> being :: Entity -> Maker () -> Maker ()
> being who x = do
>   old     <- use sender
>   sender  .= who
>   y       <- x
>   sender  .= old
>   return y

%endif

\chapter{Act framework}
\label{chapter:monad}

%if 0

\section{Act descriptions}

We define the Maker act vocabulary as a data type to represent invocations.

> data Act =
>      Bite     (Id Urn)       |  Draw     (Id Urn)  Wad          |  Form     (Id Ilk)  (Id Jar)
>   |  Free     (Id Urn)  Wad  |  Frob     Ray                    |  Give     (Id Urn)  Entity
>   |  Grab     (Id Urn)       |  Lock     (Id Urn)  Wad
>   |  Loot     Wad            |  Mark     (Id Jar)  Wad  Sec     |  Open     (Id Urn)  (Id Ilk)
>   |  Prod                    |  Pull     (Id Jar)  Entity Wad  |  Shut     (Id Urn)
>   |  Tell     Wad            |  Wipe     (Id Urn)  Wad
>   |  Mine (Id Jar)  | Hand Address Wad (Id Jar) | Sire Address
>   |  Addr Address    | Warp     Sec
>   |  Cork (Id Ilk) Wad
>  deriving (Eq, Show)

%endif

The reader does not need any abstract understanding of monads to
understand the code.  They give us a nice syntax---the |do| block
notation---for expressing exceptions and state in a way that is still
purely functional.  Each line of such a block is interpreted by the
monad to provide the semantics we want.

\section{The |Maker| monad}
\label{section:maker-monad}

This defines the |Maker| monad as a simple composition of a state
monad and an error monad:

> type Maker a = StateT System (Except Error) a

We divide act failure modes into general assertion failures and
authentication failures.

> data Error = AssertError Act | AuthError
>   deriving (Show, Eq)

An act can be executed on a given initial system state using |exec|.
The result is either an error or a new state.  The |exec| function can
also accept a sequence of acts, which will be interpreted as a
single transaction.

> exec :: System -> Maker () -> Either Error System
> exec sys m = runExcept (execStateT m sys)

\section{Asserting}

We now define a set of functions that fail unless some condition holds.

> -- General assertion
> aver x = unless x (throwError (AssertError ?act))
>
> -- Assert that an indexed value is not present
> none x = preuse x >>= \case
>   Nothing -> return ()
>   Just _  -> throwError (AssertError ?act)
>
> -- Assert that an indexed value is present
> look f = preuse f >>= \case
>   Nothing -> throwError (AssertError ?act)
>   Just x  -> return x
>
> -- Execute an act and assert a condition on its result
> want m p = m >>= (aver . p)

We define |owns id_urn id_lad| as an assertion that the given |cdp| is
owned by the given account.

> owns id_urn id_lad = do
> 
>   want (look (vat . urns . ix id_urn . lad)) (== id_lad)

We define |auth k| as an act modifier that executes |k| only if the
sender is authorized.

> auth continue = do
>   s <- use sender
>   unless (s == Account id_god) (throwError AuthError)
>   continue

%if 0

\chapter{Testing}

Sketches for property stuff...

> {-
> data Parameter =
>      Wut | Par | Way
>
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
>
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
>
> also :: Lens' s a -> Lens' s b -> Lens' s (a, b)
> also f g = lens getter setter
>   where
>   getter x = (view f x, view g x)
>   setter x (a, b) = set f a (set g b x)
>
> keeps :: Parameter -> Maker () -> System -> Bool
> keeps Wut  = maintains (vat . vox . wut)
> keeps Par  = maintains (vat . vox . par)
> keeps Way  = maintains (vat . vox . way)
> -}

Thus:

> {- foo sys0 = all (\f -> f sys0)
>   [changesOnly (  (vat . vox . par) `also`
>                   (vat . vox . way))
>      (perform Prod)] -}

%endif

\appendix

%include Maker/Prelude.lhs
%include Maker/Decimal.lhs

\end{document}
