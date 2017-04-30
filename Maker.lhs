%include preamble.fmt

\chapter{Introduction}

The \textsc{dai system}, henceforth also ``Maker,'' is a network of
Ethereum contracts defining a token that is subject to a decentralized
price stability mechanism.  The token is named |dai|.

For an overview of the economics of the system, as well as
descriptions of governance, off-chain mechanisms, and so on, see the
white\-paper.

This document is an executable technical specification of the Maker
smart contracts.  It is a draft; be aware that the contents will
certainly change before launch.

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
\item that the total dai supply is fully accounted for;
\item that acts are restricted with respect to risk stage;
\end{itemize}

along with similar invariants and conditions.  A future revision of
this document will include formal statements of these properties.

\part{Implementation}

\chapter{Preamble}

%if false

> {-# Language AllowAmbiguousTypes #-}
> {-# Language ConstraintKinds #-}
> {-# Language DeriveGeneric #-}
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
> import Data.Aeson.Types (fieldLabelModifier, defaultOptions, genericParseJSON, genericToJSON)
> import Prelude (drop)

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

> newtype Id a = Id String
>   deriving (Eq, Ord, Show)

We define another type for representing Ethereum account addresses.

> newtype Address = Address String
>   deriving (Eq, Ord, Show)

We also define the different kinds of tokens used by the system.

> data Gem  =  Gem String  -- Some assetcoin
>           |  DAI         -- Stablecoin
>           |  SIN         -- Anticoin
>           |  MKR         -- Countercoin
>
>   deriving (Eq, Ord, Show)

%if 0

> instance Read (Id a) where
>   readsPrec n s = fmap (first Id) (readsPrec n s)

> deriving instance Generic Wad
> deriving instance Generic Ray
> deriving instance Generic Sec
> deriving instance Generic (Id a)
> deriving instance Generic Address
> deriving instance Generic Gem


%endif

\section{|Tag| --- external token price data}
\actentry{|tag|}{market price of token}
\actentry{|zzz|}{expiration time of token price feed}

The data received from price feeds is categorized by token and stored
in |Tag| records.

> data Tag = Tag {
>
>     _tag  :: Wad,  -- Market price denominated in |sdr|
>     _zzz  :: Sec   -- Time of price expiration
>
>   } deriving (Eq, Show)

\section{|Entity| --- token balance holder}

We use a data type to explicitly distinguish the different entities
that can hold a token balance or invoke acts.

> data Entity  =  Account Address  -- External holder
>              |  Jar              -- Token vault
>              |  Jug              -- Mints stablecoin/anticoin, holds anticoin
> 
>              |  Vow              -- Settler
>              |  Flipper          -- Assetcoin auctioneer
>              |  Flapper          -- Stablecoin auctioneer
>              |  Flopper          -- Countercoin auctioneer
> 
>              |  Toy              -- Test driver
>              |  God              -- Omnipotent actor
> 
>   deriving (Eq, Ord, Show)

\section{|Ilk| --- urn type}
\actentry{|gem|}{assetcoin identifier}
\actentry{|mat|}{liquidation ratio}
\actentry{|axe|}{liquidation penalty}
\actentry{|hat|}{issuance ceiling}
\actentry{|tax|}{stability fee factor}
\actentry{|lax|}{price feed limbo duration}
\actentry{|rho|}{timestamp of fee unit adjustment}
\actentry{|rum|}{total outstanding art in fee unit}
\actentry{|chi|}{value of fee unit in stablecoin}

Each urn belongs to an urn type, specified by an |Ilk| record.
Five parameters, |mat|, |axe|, |hat|, |tax| and |lax|, are set by
governance and are known as the \emph{risk parameters}. The rest of
the values are used by the system to keep track of the current state.
The meaning of each |ilk| parameter is defined by its interactions in
the act definitions of Chapter~\ref{chapter:acts}; see the whitepaper
for an overview.

> data Ilk = Ilk {
>
>     _gem  :: Gem,     -- Assetcoin identifier
> 
>     _lax  :: Sec,     -- Grace period after price feed becomes unavailable
>     _mat  :: Ray,     -- Urn riddance ratio of locked value to issued value
>     _axe  :: Ray,     -- Urn riddance penalty as fraction of urn issuance
>     _hat  :: Wad,     -- Maximum total issuance for ilk (``issuance ceiling'')
>     _tax  :: Ray,     -- Stability fee as per-second fraction of urn issuance
> 
>     _chi  :: Ray,     -- Value of fee unit in stablecoin
>     _rho  :: Sec,     -- Time of latest fee unit adjustment
>     _rum  :: Wad      -- Total ilk issuance denominated in fee unit
>
>   } deriving (Eq, Show)

\section{|Urn| --- stablecoin issuance account}
\actentry{|cat|}{address of riddance initiator} \actentry{|lad|}{urn
owner} \actentry{|ilk|}{urn type} \actentry{|art|}{stablecoin issuance
in fee unit} \actentry{|ink|}{amount of locked assetcoin}
An |urn| record defines a basic entity through which users interact
with the system to issue stablecoin.  Each urn belongs to an ilk.
The urn records the value of locked assetcoin along with the amount of
stablecoin created for this particular urn.  When riddance is
triggered on an urn, the identity of the triggering entity is
also recorded.

> data Urn = Urn {
>
>     _ilk  :: Id Ilk,         -- Urn type
>     _lad  :: Entity,         -- Urn owner
> 
>     _art  :: Wad,            -- Stablecoin issuance in fee unit
>     _ink  :: Wad,            -- Amount of locked assetcoin
> 
>     _cat  :: Maybe Entity    -- Entity that triggered riddance, if applicable
>
>   } deriving (Eq, Show)

\section{|Vox| --- feedback mechanism data}
\actentry{|wut|}{|sdr| market price of stablecoin}
\actentry{|par|}{|sdr| target price of stablecoin}
\actentry{|how|}{sensitivity parameter}
\actentry{|way|}{rate of target price change}
\actentry{|tau|}{time of latest feedback cycle}

The \emph{feedback mechanism} is the aspect of the system that adjusts
the target price of dai based on market price. Its data is grouped in
a record called |Vox|.

> data Vox = Vox {
> 
>     _wut   :: Wad,    -- Stablecoin market price denominated in |sdr|
>     _par   :: Wad,    -- Stablecoin target price denominated in |sdr|
>     _way   :: Ray,    -- Current per-second change in target price
> 
>     _how   :: Ray,    -- Sensitivity parameter set by governance
>     _tau   :: Sec     -- Timestamp of latest feedback iteration
> 
>  } deriving (Eq, Show)

\section{|Vat| --- system root}

The |Vat| record aggregates the records of tokens, urns, ilks, and
price feeds, along with the data of the feedback mechanism.

> data Vat = Vat {
> 
>     _tags  :: Map Gem Tag,        -- Assetcoin price feeds
>     _ilks  :: Map (Id Ilk) Ilk,   -- Urn type records
>     _urns  :: Map (Id Urn) Urn,   -- Urn records
> 
>     _vox   :: Vox                 -- Feedback mechanism data
> 
>   } deriving (Eq, Show)

\section{System model}
\actentry{|era|}{current time}

Finally we define a record with no direct counterpart in the Solidity
contracts, which has the |Vat| record along with model state.

> data System =  System {
>
>     _balances  :: Map (Entity, Gem) Wad,  -- Token balances
>     _vat       :: Vat,                    -- Root Maker entity
>     _era       :: Sec,                    -- Current time stamp
>     _sender    :: Entity,                 -- Sender of current act
>     _accounts  :: [Address],              -- For test suites
>     _mode      :: Mode                    -- Vow operation mode
>
>   } deriving (Eq, Show)

> data Mode = Dummy
>   deriving (Eq, Show)

%if 0

> deriving instance Generic Tag
> deriving instance Generic Entity
> deriving instance Generic Ilk
> deriving instance Generic Urn
> deriving instance Generic Vox
> deriving instance Generic Vat
> deriving instance Generic System
> deriving instance Generic Mode

> instance HasResolution a => ToJSON (Decimal a) where
>   toJSON (D x) = toJSON (show x)
> 
> instance HasResolution a => FromJSON (Decimal a) where
>   parseJSON v = fmap (D . read) (parseJSON v)
>
> instance ToJSON Sec where
>   toJSON (Sec x) = toJSON (show x)
> 
> instance FromJSON Sec where
>   parseJSON v = fmap (Sec . read) (parseJSON v)
>
> instance ToJSONKey Gem    ; instance FromJSONKey Gem
> instance ToJSONKey Entity ; instance FromJSONKey Entity
> instance ToJSONKey (Id a) ; instance FromJSONKey (Id a)
> 
> instance ToJSON Wad
> instance FromJSON Wad
> instance ToJSON Ray
> instance FromJSON Ray
> instance ToJSON Address
> instance FromJSON Address
> instance ToJSON (Id a)
> instance FromJSON (Id a)
> instance ToJSON Entity
> instance FromJSON Entity
> instance ToJSON Gem
> instance FromJSON Gem
> instance ToJSON Tag where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Tag where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Ilk where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Ilk where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Urn where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Urn where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Vox where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Vox where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Vat where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Vat where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Mode
> instance FromJSON Mode
> instance ToJSON System where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON System where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }

\section*{Lens fields}

> makeLenses ''Tag  ; makeLenses ''Ilk
> makeLenses ''Urn    ; makeLenses ''Vox  ; makeLenses ''Vat
> makeLenses ''System

> balance id_gem entity = balances . ix (entity, id_gem)

%endif

\section{Default data}

> defaultIlk :: Gem -> Ilk
> defaultIlk id_gem = Ilk {
>   _gem  = id_gem,
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

> initialTag :: Tag
> initialTag = Tag {
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
>   _tags  = empty
> }

> initialSystem :: Ray -> System
> initialSystem how0 = System {
>   _balances  = empty,
>   _vat       = initialVat how0,
>   _era       = 0,
>   _sender    = God,
>   _accounts  = mempty,
>   _mode      = Dummy
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

\actentry{|feel|}{identify urn risk stage}

In order to prohibit urn acts based on risk situation, we define
these stages of risk.

> data Stage  =  Pride |  Anger |  Worry |  Panic |  Grief |  Dread
> 
>   deriving (Eq, Show)

%if 0

> deriving instance Generic Stage

%endif

We define the function |analyze| that determines the risk stage of an urn.

> analyze era0 par0 urn0 ilk0 tag0 =
>   if  | view cat  urn0  /= Nothing && view ink urn0 == 0
>         -- Riddance triggered and started
>          -> Dread
>       | view cat  urn0  /= Nothing
>         -- Riddance triggered
>          -> Grief  
>       | pro < min
>         -- Locked assetcoin value below issuance times riddance ratio
>          -> Panic  
>       | view zzz tag0 + view lax ilk0 < era0
>         -- Assetcoin price limbo exceeded limit
>          -> Panic  
>       | view zzz tag0 < era0
>         -- Assetcoin price feed in limbo
>          -> Worry  
>       | cap  > view hat ilk0
>         -- Issuance ceiling exceeded
>          -> Anger
>       | otherwise   
>         -- No problems
>          -> Pride
>
>   where
>   -- Value of urn's locked assetcoin in |sdr|:
>     pro  = view ink urn0  * view tag tag0
>
>   -- Ilk's total stablecoin issuance in |dai|:
>     cap  = view rum ilk0  * cast (view chi ilk0)
>
>   -- Urn's stablecoin issuance denominated in |sdr|:
>     con  = view art urn0  * cast (view chi ilk0) * par0
>
>   -- Required assetcoin value as per riddance ratio:
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
\caption{Urn acts and risk stages}\label{table:stages}
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
\yep & allowed for owner if able to pay \\
\meh & allowed for owner if above riddance ratio \\
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
> -- Update fee unit and unprocessed fee revenue
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   drip id_ilk
>
> -- Read parameters for risk analysis
>   era0    <- use   era
>   par0    <- use   (vat . vox . par)
>   urn0    <- look  (vat . urns . ix id_urn)
>   ilk0    <- look  (vat . ilks . ix (view ilk urn0  ))
>   tag0    <- look  (vat . tags . ix (view gem ilk0  ))
>
> -- Return risk stage of urn
>   return (analyze era0 par0 urn0 ilk0 tag0)

Urn acts use |feel| to prohibit increasing risk when already risky,
and to freeze stablecoin and assetcoin during riddance; see
Table~\ref{table:stages}.

\clearpage
\section{Issuance}

\actentry{|open|}{create urn} Any user can open one or more accounts
with the system using |open|, specifying a self-chosen account
identifier and an ilk.

> open id_urn id_ilk = do
>
> -- Fail if account identifier is taken
>   none (vat . urns . ix id_urn)
>
> -- Fail if ilk type is not present
>   _ <- look (vat . ilks . ix id_ilk)
>
> -- Create an urn with the sender as owner
>   id_lad <- use sender
>   initialize (vat . urns . at id_urn) (emptyUrn id_ilk id_lad)

\actentry{|give|}{change urn owner} The owner of an urn can
transfer its ownership at any time using |give|.

> give id_urn id_lad = do
>
> -- Fail if sender is not the urn owner
>   id_sender <- use sender
>   owns id_urn id_sender
>
> -- Transfer urn ownership
>   vat . urns . ix id_urn . lad .= id_lad

\actentry{|lock|}{lock assetcoin}Unless urn is in riddance, its owner
can use |lock| to lock more assetcoin.

> lock id_urn wad_gem = do
>
> -- Fail if sender is not the urn owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Fail if riddance in process
>   want (feel id_urn) (`notElem` [Grief, Dread])
>
> -- Identify assetcoin
>   id_ilk  <- look (vat . urns . ix id_urn  . ilk)
>   id_gem  <- look (vat . ilks . ix id_ilk  . gem)
>
> -- Take custody of assetcoin
>   transfer id_gem wad_gem id_lad Jar
>
> -- Record an assetcoin balance increase
>   increase (vat . urns . ix id_urn . ink) wad_gem

\clearpage

\actentry{|free|}{reclaim assetcoin}When an urn has no risk
problems (except that its ilk's ceiling may be exceeded),
its owner can use |free| to reclaim some amount of assetcoin, as
long as this would not take the urn below its riddance ratio.

> free id_urn wad_gem = do
>
> -- Fail if sender is not the urn owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Record an assetcoin balance decrease
>   decrease (vat . urns . ix id_urn . ink) wad_gem
>
> -- Roll back on any risk problem except ilk ceiling excess
>   want (feel id_urn) (`elem` [Pride, Anger])
>
> -- Release custody of assetcoin quantity
>   id_ilk  <- look (vat . urns . ix id_urn . ilk)
>   id_gem  <- look (vat . ilks . ix id_ilk . gem)
>   transfer id_gem wad_gem Jar id_lad

\actentry{|draw|}{issue stablecoin}When an urn has no risk problems,
its owner can can use |draw| to issue fresh stablecoin,
as long as the ilk ceiling is not exceeded and the issuance
would not take the urn below its riddance ratio.

> draw id_urn wad_dai = do
>
> -- Fail if sender is not the urn owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Update fee unit and unprocessed fee revenue
>   id_ilk     <- look (vat . urns . ix id_urn . ilk)
>   chi1       <- drip id_ilk
>
> -- Denominate issuance quantity in fee unit
>   let  wad_chi = wad_dai / cast chi1
>
> -- Record increase of urn's stablecoin issuance
>   increase (vat . urns . ix id_urn . art) wad_chi
>
> -- Record increase of ilk's stablecoin issuance
>   increase (vat . ilks . ix id_ilk . rum) wad_chi
>
> -- Roll back on any risk problem
>   want (feel id_urn) (== Pride)
>
> -- Mint both stablecoin and anticoin
>   lend wad_dai
>
> -- Transfer stablecoin to urn owner
>   transfer DAI wad_dai Jug id_lad

\actentry{|wipe|}{reverse stablecoin issuance}An urn owner who has
previously issued stablecoin can use |wipe| to send back dai and
reduce the urn's issuance.

> wipe id_urn wad_dai = do
>
> -- Fail if sender is not the urn owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
> -- Fail if urn is in riddance
>   want (feel id_urn) (`notElem` [Grief, Dread])
>
> -- Update fee unit and unprocessed fee revenue
>   id_ilk <- look (vat . urns . ix id_urn . ilk)
>   chi1   <- drip id_ilk
>
> -- Denominate stablecoin amount in fee unit
>   let  wad_chi = wad_dai / cast chi1
>
> -- Record decrease of urn issuance
>   decrease (vat . urns . ix id_urn . art) wad_chi
>
> -- Record decrease of ilk total issuance
>   decrease (vat . ilks . ix id_ilk . rum) wad_chi
>
> -- Take custody of stablecoin from urn owner
>   transfer DAI wad_dai id_lad Jar
>
> -- Destroy stablecoin and anticoin
>   mend wad_dai

\actentry{|shut|}{wipe, free, and delete urn}An urn owner can use
|shut| to close their account---reversing all issuance plus fee and
reclaiming all assetcoin---if the price feed is up to date and the urn
is not in riddance.

> shut id_urn = do
>
>   -- Update fee unit and unprocessed fee revenue
>     id_ilk <- look (vat . urns . ix id_urn . ilk)
>     chi1   <- drip id_ilk
>
>   -- Reverse all issued stablecoin plus fee
>     art0 <- look (vat . urns . ix id_urn . art)
>     wipe id_urn (art0 * cast chi1)
>
>   -- Reclaim all locked assetcoin
>     ink0 <- look (vat . urns . ix id_urn . ink)
>     free id_urn ink0
>
>   -- Nullify urn record
>     vat . urns . at id_urn .= Nothing

\clearpage
\section{Adjustment}

\actentry{|prod|}{adjust target price and target rate}

The feedback mechanism is updated through |prod|, which can be invoked
at any time by keepers, but is also invoked as a side effect of any
urn act that uses |feel| to assess risk.

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
\actentry{|drip|}{update fee unit and unprocessed fee revenue}

The stability fee of an ilk can change through governance.  Due to the
constraint that acts should run in constant time, the system cannot
iterate over urns to effect such changes.  Instead each ilk has a
single ``fee unit'' which accumulates the stability fee.  The |drip|
act updates this unit.  It can be called at any time by keepers, but
is also called as a side effect of every act that uses |feel| to
assess urn risk.

> drip id_ilk = do
>
> -- Time stamp of previous |drip|
>   rho0  <- look (vat . ilks . ix id_ilk . rho)
> -- Current stability fee
>   tax0  <- look (vat . ilks . ix id_ilk . tax)
> -- Current fee unit value
>   chi0  <- look (vat . ilks . ix id_ilk . chi)
> -- Current total issuance in fee unit
>   rum0  <- look (vat . ilks . ix id_ilk . rum)
> -- Current time stamp
>   era0  <- use era
>
>   let
>   -- Time difference in seconds
>     age   = era0 - rho0
>   -- Value of fee unit increased according to stability fee
>     chi1  = chi0 * tax0 ^^ age
>   -- Stability fee revenue denominated in new unit
>     dew   = (cast (chi1 - chi0) :: Wad) * rum0
>
> -- Mint stablecoin and anticoin for marginally accrued fee
>   lend dew
>
> -- Record time of update
>   vat . ilks . ix id_ilk . rho  .= era0
> -- Record new fee unit
>   vat . ilks . ix id_ilk . chi  .= chi1
>
> -- Return the new fee unit
>   return chi1

\clearpage
\section{Price feed input}

\actentry{|mark|}{update market price of assetcoin}The |mark|
act records a new market price of an assetcoin along with the
expiration date of this price.

> mark id_gem tag1 zzz1 = auth $ do
>     initialize (vat . tags . at id_gem) Tag {
>       _tag  = tag1,
>       _zzz  = zzz1
>       }

\actentry{|tell|}{update market price of stablecoin}The |tell| act
records a new market price of the |dai| token along with the
expiration date of this price.

> tell wad = auth $ do vat . vox . wut .= wad

\section{Riddance}

\actentry{|bite|}{mark for riddance}%
When an urn's stage marks it
  as in need of riddance,
 any account can invoke the |bite| act
  to trigger the riddance process.
This enables the settler contract
 to grab the assetcoin for auctioning
 and take over the anticoin.

> bite id_urn = do
>
>   -- Fail if urn is not in the appropriate stage
>     want (feel id_urn) (== Panic)
>
>   -- Record the sender as the riddance initiator
>     id_cat     <- use sender
>     vat . urns . ix id_urn . cat  .= Just id_cat
>
>   -- Apply riddance penalty to urn issuance
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     axe0    <- look (vat . ilks . ix id_ilk . axe)
>     art0    <- look (vat . urns . ix id_urn . art)
>     let art1 = art0 * cast axe0
> 
>   -- Update urn issuance to include penalty
>     vat . urns . ix id_urn . art   .=  art1

\clearpage

\actentry{|grab|}{take stablecoin and anticoin for riddance}%
After riddance has been triggered,
the designated settler contract invokes |grab|
to receive both the urn's assetcoin
and the anticoins
corresponding to the urn's issuance.

> grab id_urn = auth $ do
>
>   -- Fail if urn is not marked for riddance
>     want (feel id_urn) (== Grief)
>
>     ink0    <- look (vat . urns . ix id_urn . ink)
>     art0    <- look (vat . urns . ix id_urn . art)
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     id_gem  <- look (vat . ilks . ix id_ilk . gem)
> 
>   -- Update the fee unit and unprocessed fee revenue
>     chi1    <- drip id_ilk
>
>   -- Denominate the issuance in dai
>     let con = art0 * cast chi1
>
>   -- Transfer assetcoin and anticoin to settler
>     transfer id_gem  ink0  Jar Vow
>     transfer SIN     con   Jar Vow
> 
>   -- Nullify urn's assetcoin and anticoin quantities
>     vat . urns . ix id_urn . ink .= 0
>     vat . urns . ix id_urn . art .= 0
>
>   -- Decrease the ilk's total issuance
>     decrease (vat . ilks . ix id_ilk . rum) art0

\actentry{|plop|}{finish riddance returning profit}When the settler
has finished the riddance of an urn, it invokes |plop| to give back
any assetcoin it did not need to sell and restore the urn.

> plop id_urn wad_dai = auth $ do
>
>   -- Fail unless urn is in the proper stage
>     want (feel id_urn) (== Dread)
>
>   -- Forget the urn's initiator of riddance
>     vat . urns . ix id_urn . cat .= Nothing
>
>   -- Take excess assetcoin from settler to vault
>     id_vow  <- use sender
>     id_ilk  <- look (vat . urns . ix id_urn . ilk)
>     id_gem  <- look (vat . ilks . ix id_ilk . gem)
>     transfer id_gem wad_dai id_vow Jar
>
>   -- Record the excess assetcoin as belonging to the urn
>     vat . urns . ix id_urn . ink .= wad_dai

\clearpage

\actentry{|loot|}{take unprocessed stability fees}%
The settler can invoke |loot| at any time
to claim all uncollected stability fee revenue
for use in the countercoin buy-and-burn auction.

> loot = auth $ do
>
> -- The dai vault's balance is the uncollected stability fee revenue
>   wad  <- look (balance DAI Jar)
>
> -- Transfer the entire dai vault balance to sender
>   transfer DAI wad Jar Vow

\section{Auctioning}
\actentry{|flip|}{put assetcoin up for riddance auction}%

> flip id_gem wad_jam wad_tab id_urn = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

\actentry{|flap|}{put fee revenue stablecoin up for auction}%

> flap = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

\actentry{|flop|}{inflate countercoin for auction}%

> flop = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

\clearpage
\section{Settlement}
\actentry{|tidy|}{burn equal quantities of stablecoin and anticoin}%

> tidy who = auth $ do
>
> -- Find the entity's stablecoin and anticoin balances
>   awe  <- look (balance DAI  who)
>   woe  <- look (balance SIN  who)
>
> -- We can burn at most the smallest of the two balances
>   let x = min awe woe
>
> -- Transfer stablecoin and anticoin to the settler
>   transfer  DAI  x who  Vow
>   transfer  SIN  x who  Vow
>
> -- Burn both stablecoin and anticoin
>   burn      DAI  x      Vow
>   burn      SIN  x      Vow

\actentry{|kick|}{flap, flop, and whatnot}

> kick = do
>
> -- Transfer unprocessed stability fee revenue to vow account
>   loot
> 
> -- Cancel stablecoin against anticoin
>   tidy Vow
>
> -- Assign any remaining stablecoin to countercoin-deflating auction
>   transferAll DAI Vow Flapper
>   flap
>
> -- Assign any remaining anticoin to countercoin-inflating auction
>   transferAll SIN Vow Flopper
>   flop

\section{Governance}

\actentry{|form|}{create a new ilk}Governance uses |form| to create a
new ilk.  Since the new type is initialized with a zero ceiling, a
separate transaction can safely set the risk parameters before any
issuance occurs.

> form id_ilk id_gem = auth $ do
>     initialize (vat . ilks . at id_ilk) (defaultIlk id_gem)

\actentry{|frob|}{set the sensitivity parameter}Governance uses |frob|
to alter the sensitivity factor, which is the only mutable parameter
of the feedback mechanism.

> frob how1 = auth $ do vat . vox . how .= how1

\actentry{|chop|}{set riddance penalty}%
\actentry{|cork|}{set ilk ceiling}%
\actentry{|calm|}{set limbo duration}%
\actentry{|cuff|}{set riddance ratio}%
Governance can alter the five risk parameters of an ilk using
|cuff| for the riddance ratio; |chop| for the riddance penalty;
|cork| for the ilk ceiling; |calm| for the duration of
price limbo; and |crop| for the stability fee.

> cuff id_ilk mat1  = auth $ do vat . ilks . ix id_ilk . mat  .= mat1
> chop id_ilk axe1  = auth $ do vat . ilks . ix id_ilk . axe  .= axe1
> cork id_ilk hat1  = auth $ do vat . ilks . ix id_ilk . hat  .= hat1
> calm id_ilk lax1  = auth $ do vat . ilks . ix id_ilk . lax  .= lax1

\actentry{|crop|}{set stability fee}%
When altering the stability fee with |crop|, we ensure that the
previous stability fee has been accounted for in the internal
fee unit.

> crop id_ilk tax1 = auth $ do
> 
>   -- Apply the current stability fee to the internal fee unit
>     drip id_ilk
>   -- Change the stability fee
>     vat . ilks . ix id_ilk . tax .= tax1

\section{Token manipulation}%
We model the |erc20| transfer function in simplified form (omitting
the concept of ``allowance'').

> transfer id_gem wad src dst  =
>
> -- Operate in the token's balance table
>   zoom balances $ do
>
>   -- Fail if source balance insufficient
>     balance <- look (ix (src, id_gem))
>     aver (balance >= wad)
>
>   -- Update balances
>     decrease    (ix (src, id_gem))  wad
>     initialize  (at (dst, id_gem))  0
>     increase    (ix (dst, id_gem))  wad

> transferAll id_gem src dst = do
>   wad <- look (balance id_gem src)
>   transfer id_gem wad src dst

\actentry{|mint|}{inflate token}%
The internal act |mint| inflates the supply of a token.
It is used by |lend| to create new stablecoin and anticoin,
and by the settler to create new countercoin.

> mint id_gem wad dst = do
>   initialize  (balances . at (dst, id_gem))  0
>   increase    (balances . ix (dst, id_gem))  wad

\actentry{|burn|}{deflate token}%
The internal act |burn| deflates the supply of a token.
It is used by |mend| to destroy stablecoin and anticoin,
and by the settler to destroy countercoin.

> burn id_gem wad src =
>   decrease (balances . ix (src, id_gem)) wad

\actentry{|lend|}{mint stablecoin and anticoin}%
The internal act |lend| mints identical amounts
of both stablecoin and anticoin.
It is used by |draw| to issue stablecoin;
it is also used by |drip| to issue stablecoin
representing revenue from stability fees,
which stays in the vault until collected.

> lend wad_dai = do
>
>   mint DAI wad_dai Jug
>   mint SIN wad_dai Jug

\actentry{|mend|}{burn dai and debt token}%
The internal act |mend| destroys identical amounts
of both dai and the internal debt token.
Its use via |wipe| is how the stablecoin supply is reduced.

> mend wad_dai = do
>
>   burn DAI wad_dai Jug
>   burn SIN wad_dai Jug

%if 0

\section{Manipulation}

\actentry{|warp|}{travel through time}

> warp t = auth (do increase era t)

\actentry{|mine|}{create toy token type}

> mine id_gem = do
>
>     initialize (balances . at (Toy, id_gem)) 1000000000000

\actentry{|hand|}{give toy tokens to account}

> hand dst wad_gem id_gem = do
>   transfer id_gem wad_gem
>     Toy (Account dst)

\actentry{|sire|}{register a new toy account}

> sire lad = do prepend accounts lad

\section{Other stuff}

> perform :: Act -> Maker ()
> perform x =
>   let ?act = x in case x of
>     Form id gem      -> form id gem
>     Mark gem tag zzz -> mark gem tag zzz
>     Open id ilk      -> open id ilk
>     Tell wad         -> tell wad
>     Frob ray         -> frob ray
>     Prod             -> prod
>     Drip x           -> drip x
>     Warp t           -> warp t
>     Give urn lad     -> give urn lad
>     Lock urn wad     -> lock urn wad
>     Mine id          -> mine id
>     Hand lad wad gem -> hand lad wad gem
>     Sire lad         -> sire lad
>     Draw lad wad     -> draw lad wad
>     Cuff ilk ray     -> cuff ilk ray
>     Chop ilk ray     -> chop ilk ray
>     Cork ilk wad     -> cork ilk wad
>     Calm ilk sec     -> calm ilk sec
>     Mint gem wad lad -> mint gem wad lad

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
>      Bite     (Id Urn)
>   |  Draw     (Id Urn)  Wad
>   |  Form     (Id Ilk)  (Gem)
>   |  Free     (Id Urn)  Wad
>   |  Frob     Ray
>   |  Give     (Id Urn)  Entity
>   |  Grab     (Id Urn)
>   |  Lock     (Id Urn)  Wad
>   |  Loot     Wad
>   |  Mark     (Gem)  Wad  Sec
>   |  Open     (Id Urn)  (Id Ilk)
>   |  Prod
>   |  Shut     (Id Urn)
>   |  Tell     Wad
>   |  Wipe     (Id Urn)  Wad
>   |  Mine (Gem)
>   |  Hand Address Wad (Gem)
>   |  Sire Address
>   |  Addr Address
>   |  Warp Sec
>   |  Cuff (Id Ilk) Ray
>   |  Chop (Id Ilk) Ray
>   |  Cork (Id Ilk) Wad
>   |  Calm (Id Ilk) Sec
>   |  Mint Gem Wad Entity
>   |  Drip (Id Ilk)
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
>   unless (s == God) (throwError AuthError)
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
