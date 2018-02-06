<!doctype html>
<meta charset=utf-8>
<link rel=stylesheet href=maker.css>

<title>Dai Stablecoin Purple Paper</title>

<header>
<center>
<img src=makerdao.svg>
<p class=x2>REFERENCE<br>IMPLEMENTATION</p>
<p class=x5>of the decentralized</p>
<p class=x4>DAI STABLECOIN</p>
<p class=x5>issuance system</p>
<p class=x6>Nikolai Mushegian<br>Daniel Brockman<br>Mikael Brockman</p>
<p class=x7>[draft; %date%]</p>
<p class=x8>◈</p>
</center>
</header>

<div style="font-size: 180%">Contents</div>
<toc>
</toc>

<section>
<h1>Introduction</h1>

<p>The <b>Dai stablecoin system</b> is a set of blockchain smart
contracts designed to issue a collateral-backed token (called the dai)
and subject its price to a decentralized stability mechanism.

<p>This document is an executable technical specification of the
system.  It is a draft and will change before launch.

<p>For an overview of the system, see the <a
href="https://github.com/makerdao/docs/blob/master/Dai.md">white
paper</a>.

<p>For a "choose your own adventure" exploration of the system's
mechanics, please wait for the interactive FAQ.

<p>We are dedicated to providing material for new people to understand
the system in depth.  This will be important for successful governance
in the project's future.

<p>If you have any questions, ask on our <a
href="https://chat.makerdao.com">chat</a> or <a
href="https://reddit.com/r/MakerDAO">subreddit</a>.  Asking helps us
work on our explanatory material, so we appreciate it.

</section><section>

<h2>Why a reference implementation?</h2>

<p>The contracts that will be deployed on the Ethereum blockchain are
prototyped in Solidity.  This paper is a model of the system written
as a Haskell program.  The motivations for this include:

<p><b>Comparison.</b>
Checking two free-standing implementations
against each other is a well-known way of ensuring that they both
behave as intended.

<p><b>Verification.</b> Haskell lets us use powerful testing tools
such as QuickCheck for comprehensively verifying key properties. This
is a middle ground between testing and formal verification.

<p><b>Formality.</b> The work of translating into a purely functional
program opens up opportunities for formal verification.  This document
will be useful for modelling aspects of the system in a proof
assistant like Isabelle.

<p><b>Explicitness.</b> Coding the contract behavior in Haskell, a
statically typed functional language, enforces explicit description of
aspects which Solidity leaves implicit.

<p><b>Clarity.</b>
An implementation not intended to be deployed
on the blockchain is free from concerns about optimizing for gas cost
and other factors that make the Solidity implementation less ideal as
an understandable specification.

<p><b>Simulation.</b> Solidity is specific to the blockchain
environment and lacks facilities for interfacing with files or other
programs.  A reference implementation is useful for doing simulations
of the system's economic, game-theoretic, or statistical aspects.

<h2>Formal verification and steps thereto</h2>

<p>We are developing automatic test suites that generate interaction
sequences for property verification.

<p>One such property is that the reference implementation behaves like
the on-chain implementation. We verify this by generating Solidity
test cases with equality assertions for the entire state.

<p>Other key properties include

<ul>
<li>that the target price changes according to the target rate;
<li>that the total dai supply is fully accounted for;
<li>that actions are restricted with respect to CDP stage;
</ul>

<p>along with similar invariants and conditions.  A future revision of
this document will include formal statements of these properties.

<h2>Note on jargon</h2>

<p>The reference implementation uses a concise vocabulary for system
variables and actions.

<p>This document has a glossary accessible through hovering over
highlighted words.

<p>Here are some of the motivations for this jargon:

<p><ul>

<li>We sidestep terminological debates;
  for example, whether to say
  &raquo;rate of target price change&laquo; or
   &raquo;target rate&laquo;.

<li>With decoupled
 financial and technical
  vocabularies,
 we can more flexibly
  improve one
   without affecting the other.

<li>The ability to discuss the system formally,
 with the financial interpretation partly suspended,
  has suggested insights that would have been
   harder to think of inside the normal language.

<li>The precise and distinctive language
 makes the structure and logic
  of the implementation
   more apparent
    and easier to formalize.

<li>Concise names make the code less verbose
 and the concepts easier to handle on paper, whiteboard, etc.

</ul>

<h1>Dai mechanics</h1>

<p><aside>Note: this section is incomplete. It is supposed to briefly
and technically explain the explicit mechanics of the system with
links to relevant definitions.</aside>

<p>The dai stablecoin system lets users lock collateral assets and
issue dai in proportion to the collateral's market value.  Thus they
can deposit their valuable tokens in order to withdraw some quantity
of stablecoin.  Such a deposit account is called a "collateralized
debt position" or CDP.

<aside>See <code>lock</code>, <code>draw</code>, and
<code>Urn</code>.</aside>

<p>As long as such a deposit retains sufficient market value, the user
may reclaim their deposit, partially or in whole, by paying back dai.
As long as the CDP is collateralized in excess of the required ratio,
the user can also decrease their collateralization by reclaiming part
of the deposit without paying back dai.

<aside>See <code>free</code> and <code>wipe</code>.</aside>

<p>Governance decides which external tokens are valid as collateral,
and creates different deposit classes, or "CDP types", each with
different parameters such as maximum dai issuance, minimum collateral
ratio, and so on.

<aside>See <code>Ilk</code>.</aside>

<p>For deciding collateral requirements, the system values the dai not
at the market price, but at its own <i>target price</i>, which is
adjusted by the stability mechanism.

<aside>See <code>feel</code>, which determines the lifecycle stage of
a CDP.</aside>

<p>The target price adjustment is a second order effect. Primarily,
the stability mechanism reacts to market price changes by adjusting
the <i>target rate</i>.

<aside>See <code>prod</code>, which updates the stability
mechanism.</aside>


<!--
<todo>Explain CDPs and CDP types.</todo>
<todo>Explain the debt unit.</todo>
<todo>Explain SIN.</todo>
<todo>Explain MKR.</todo>
<todo>Explain price tags.</todo>
<todo>Explain feedback mechanism.</todo>
-->

</section><section>
<h1>Preamble and data types</h1>

<!--

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

-->

<p>This program uses some symbols defined in external libraries.
Most symbols should be clear in context, but our "prelude" lists and
briefly explains each imported type and function.

<todo>Render the prelude.</todo>

> module Maker where
>
> import Prelude (); import Maker.Prelude; import Maker.Decimal

<!--

> import Debug.Trace
> import Data.Aeson.Types (fieldLabelModifier, defaultOptions, genericParseJSON, genericToJSON)
> import Prelude (drop)

-->

<!--
<p>Haskell syntax note: <code>newtype</code> defines a type synonym with distinct
type identity; <code>data</code> creates a record type; and <code>deriving</code> creates
automatic instances of common functionality.
-->

<h2>Numeric types</h2>

<p>The system uses two precisions of decimal numbers, to which we have
given short mnemonic names.

<p>One is called <em>wad</em> and has 18 digits of precision.  It is
used for token quantities, such as amounts of ETH, DAI, or MKR.

<p>The other is called <em>ray</em> and has 36 digits of precision.
It is used for precise rates and ratios, such as the stability
fee parameter.

<p>We define these as distinct types.  The type system will not allow
us to combine them without explicit conversion.

> newtype Wad = Wad (Decimal E18)
>   deriving (Ord, Eq, Num, Real, Fractional, RealFrac)
>
> newtype Ray = Ray (Decimal E36)
>   deriving (Ord, Eq, Num, Real, Fractional, RealFrac)

<p>We define a generic function for converting one of these types to
the other.

> cast x = fromRational (toRational x) -- [Via fractional $n/m$ form]

<!--

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

-->

<p>We also define a type for time durations in whole seconds.

> newtype Sec = Sec Int
>   deriving (Eq, Ord, Enum, Num, Real, Integral)

<h2>Identifiers and addresses</h2>

<p>The following common Haskell idiom lets us use <code>Id Ilk</code>,
<code>Id Urn</code>, and so on, as distinct identifier types.

> newtype Id a = Id String
>   deriving (Eq, Ord, Show)

<p>We define another type for representing Ethereum account addresses.

> newtype Address = Address String
>   deriving (Eq, Ord, Show)

<h2><code>Gem, SIN, DAI, MKR</code>: token identifiers</h2>

<p>The system makes use of four basic types of tokens.

> data Token
>
>     -- Some collateral token approved by system governance
>   = Gem (Id Tag)
>
>     -- Fungible stablecoin, issued by CDP owners and traded publicly
>   | DAI
>
>     -- Internal anticoin whose quantity is always equal to total issued dai
>   | SIN
>
>     -- Volatile countercoin and voting token
>   | MKR
>
>   deriving (Eq, Ord, Show)

<p>The system's approved collateral tokens are called "gems". We use
the type <code>Id Tag</code> to denote the identity of some
collateral token.

<p>The model treats all collateral tokens as basic ERC20 tokens
differing only in symbol.  In reality, voters should make sure that
tokens are well-behaved before approving them.

<h2><code>Tag</code>: collateral token price record</h2>

<p>The data received from price feeds is stored in
<code>Tag</code> records.

> data Tag = Tag {
>
>   -- Latest token market price (denominated in SDR)
>   _tag :: Wad,
>
>   -- Timestamp after which price should be considered stale
>   _zzz :: Sec
>
> } deriving (Eq, Show)

<h2><code>Urn</code>: CDP record</h2>

<p>An <code>Urn</code> record keeps track of one CDP.

> data Urn = Urn {
>
>   -- CDP type identifier
>   _ilk  :: Id Ilk,
>
>   -- CDP owner
>   _lad  :: Address,
>
>   -- Amount of outstanding dai issued by this CDP, denominated in debt unit
>   _art  :: Wad,
>
>   -- Amount of collateral currently locked by this CDP
>   _ink  :: Wad,
>
>   -- Actor that triggered liquidation, if applicable
>   _cat  :: Maybe Actor
>
> } deriving (Eq, Show)

<h2><code>Ilk</code>: CDP type record</h2>

<p>An <code>Ilk</code> record keeps track of one CDP type.

> data Ilk = Ilk {
>
>   -- Token used as collateral for CDPs of this type
>   _gem :: Id Tag,
>
>   -- Total debt owed by CDPs of this type, denominated in debt unit
>   _rum :: Wad,
>
>   -- Current dai value of debt unit, increasing according to stability fee
>   _chi :: Ray,
>
>   -- Debt ceiling: maximum total outstanding dai value that can be issued by this CDP type
>   _hat :: Wad,
>
>   -- Liquidation ratio (collateral value per dai value)
>   _mat :: Ray,
>
>   -- Liquidation penalty (fraction of dai)
>   _axe :: Ray,
>
>   -- Fee (per-second fraction of dai)
>   _tax :: Ray,
>
>   -- Grace period of price feed unavailability
>   _lax :: Sec,
>
>   -- Timestamp of latest debt unit adjustment
>   _rho :: Sec
>
> } deriving (Eq, Show)

<h2><code>Vox</code>: feedback mechanism record</h2>

<p>The <em>feedback mechanism</em> is the aspect of the system that
adjusts the target price of dai based on market price. Its data is
grouped in a record called <code>Vox</code>.

> data Vox = Vox {
>
>   -- Dai market price denominated in SDR
>   _wut :: Wad,
>
>   -- Dai target price denominated in SDR
>   _par :: Wad,
>
>   -- Current per-second change in target price
>   _way :: Ray,
>
>   -- Sensitivity parameter (set by governance)
>   _how :: Ray,
>
>   -- Timestamp of latest feedback iteration
>   _tau :: Sec
>
> } deriving (Eq, Show)

<h2><code>Actor</code>: account identifier</h2>

<p>We use a data type to explicitly distinguish the different entities
that can hold a token balance or invoke actions.

> data Actor
>
>     -- Extern address (CDP owner)
>   = Account Address
>
>     -- Collateral vault, holds all locked collateral until liquidation
>   | Jar
>
>     -- DAI and SIN are minted and burned by the "jug"
>   | Jug
>
>     -- The settler component
>   | Vow
>
>     -- The collateral auctioneer that raises DAI to cover liquidations
>   | Flipper
>
>     -- The "buy and burn" auctioneer that spends fee revenue on buying MKR
>   | Flapper
>
>     -- The "inflate and sell" auctioneer that mints MKR to cover liquidations
>   | Flopper
>
>     -- Test driver (not present in real system)
>   | Toy
>
>     -- Omnipotent actor (temporary kludge)
>   | God
>
>   deriving (Eq, Ord, Show)




<!--

> instance Read (Id a) where
>   readsPrec n s = fmap (first Id) (readsPrec n s)

> deriving instance Generic Wad
> deriving instance Generic Ray
> deriving instance Generic Sec
> deriving instance Generic (Id a)
> deriving instance Generic Address
> deriving instance Generic Token


-->

<h2>System model</h2>

<p>Finally we define the overall state of the model.

> data System = System {
>
>   -- Feedback mechanism data
>   _vox      :: Vox,
>
>   -- CDP records
>   _urns     :: Map (Id Urn) Urn,
>
>   -- CDP type records
>   _ilks     :: Map (Id Ilk) Ilk,
>
>   -- Price tags of collateral tokens
>   _tags     :: Map (Id Tag) Tag,
>
>   -- Token balances by actor and token
>   _balances :: Map (Actor, Token) Wad,
>
>   -- Current timestamp
>   _era      :: Sec,
>
>   -- Settler operation mode
>   _mode     :: Mode,
>
>   -- Sender of current action
>   _sender   :: Actor,
>
>   -- All user accounts (for tests)
>   _accounts :: [Address]
>
> } deriving (Eq, Show)

> -- Settler-related work in progress
> data Mode = Dummy
>   deriving (Eq, Show)

<!--

> deriving instance Generic Tag
> deriving instance Generic Actor
> deriving instance Generic Ilk
> deriving instance Generic Urn
> deriving instance Generic Vox
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
> instance ToJSONKey Token ; instance FromJSONKey Token
> instance ToJSONKey Actor ; instance FromJSONKey Actor
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
> instance ToJSON Actor
> instance FromJSON Actor
> instance ToJSON Token
> instance FromJSON Token
> instance ToJSON Tag where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Tag where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Ilk where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Ilk where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Urn where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Urn where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Vox where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON Vox where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance ToJSON Mode
> instance FromJSON Mode
> instance ToJSON System where { toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 } }
> instance FromJSON System where { parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 } }

<h3>Lens fields</h3>

> makeLenses ''Tag  ; makeLenses ''Ilk
> makeLenses ''Urn    ; makeLenses ''Vox
> makeLenses ''System

> balance id_gem entity = balances . ix (entity, id_gem)

-->

<h1>Actions</h1>

<p>The <em>actions</em> are the basic state transitions of the system.

<p>Unless specified as <em>internal</em>, actions are accessible as
public functions on the blockchain.

<p>The <code>auth</code> modifier marks actions which can only be
invoked from addresses to which the system has granted authority.

<p>For details on the underlying &raquo;Action monad&laquo; which
specifies how the action definitions behave with regard to state and
rollback, see chapter&nbsp;\ref{chapter:monad}.


<h2>Issuance</h2>

<p>Any user can open one or more accounts with the system using
<code>open</code>, specifying a self-chosen account identifier and
an ilk.

> open id_urn id_ilk = do
>
>  -- Fail if account identifier is taken
>   none (urns . ix id_urn)
>
>  -- Fail if ilk type is not present
>   _ <- look (ilks . ix id_ilk)
>
>  -- Create a CDP with the sender as owner
>   Account id_lad <- use sender
>   initialize (urns . at id_urn) (emptyUrn id_ilk id_lad)

<p>The owner of an urn can transfer its ownership at any time using
<code>give</code>.

> give id_urn id_lad = do
>
>  -- Fail if sender is not the CDP owner
>   id_sender <- use sender
>   owns id_urn id_sender
>
>  -- Transfer CDP ownership
>   assign (urns . ix id_urn . lad) id_lad

<p>Unless CDP is in liquidation, its owner can use <code>lock</code> to
lock more collateral.

> lock id_urn wad_gem = do
>
>  -- Fail if sender is not the CDP owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
>  -- Fail if liquidation in process
>   want (feel id_urn) (not . oneOf [Grief, Dread])
>
>  -- Identify collateral token
>   id_ilk  <- look (urns . ix id_urn . ilk)
>   id_tag  <- look (ilks . ix id_ilk . gem)
>
>  -- Take custody of collateral
>   transfer (Gem id_tag) wad_gem id_lad Jar
>
>  -- Record an collateral token balance increase
>   increase (urns . ix id_urn . ink) wad_gem

<p>When a CDP has no risk problems (except that its ilk's ceiling may
be exceeded), its owner can use <code>free</code> to reclaim some
amount of collateral, as long as this would not take the CDP below its
liquidation ratio.

> free id_urn wad_gem = do
>
>  -- Fail if sender is not the CDP owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
>  -- Record a collateral token balance decrease
>   decrease (urns . ix id_urn . ink) wad_gem
>
>  -- Roll back on any risk problem except ilk ceiling excess
>   want (feel id_urn) (oneOf [Pride, Anger])
>
>  -- Release custody of collateral
>   id_ilk <- look (urns . ix id_urn . ilk)
>   id_tag <- look (ilks . ix id_ilk . gem)
>   transfer (Gem id_tag) wad_gem Jar id_lad

<p>When a CDP has no risk problems, its owner can can use
<code>draw</code> to issue fresh stablecoin, as long as the ilk
ceiling is not exceeded and the issuance would not take the CDP below
its liquidation ratio.

> draw id_urn wad_dai = do
>
>  -- Fail if sender is not the CDP owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
>  -- Update debt unit and unprocessed fee revenue
>   id_ilk <- look (urns . ix id_urn . ilk)
>   chi1 <- drip id_ilk
>
>  -- Denominate issuance quantity in debt unit
>   let wad_chi = wad_dai / cast chi1
>
>  -- Record increase of CDP's stablecoin issuance
>   increase (urns . ix id_urn . art) wad_chi
>
>  -- Record increase of ilk's stablecoin issuance
>   increase (ilks . ix id_ilk . rum) wad_chi
>
>  -- Roll back on any risk problem
>   want (feel id_urn) (== Pride)
>
>  -- Mint both stablecoin and anticoin
>   lend wad_dai
>
>  -- Transfer stablecoin to CDP owner
>   transfer DAI wad_dai Jug id_lad

<p>An CDP owner who has previously issued stablecoin can use
<code>wipe</code> to send back dai and reduce the CDP's issuance.

> wipe id_urn wad_dai = do
>
>  -- Fail if sender is not the CDP owner
>   id_lad <- use sender
>   owns id_urn id_lad
>
>  -- Fail if CDP is in liquidation
>   want (feel id_urn) (not . oneOf [Grief, Dread])
>
>  -- Update debt unit and unprocessed fee revenue
>   id_ilk <- look (urns . ix id_urn . ilk)
>   chi1 <- drip id_ilk
>
>  -- Denominate stablecoin amount in debt unit
>   let wad_chi = wad_dai / cast chi1
>
>  -- Record decrease of CDP issuance
>   decrease (urns . ix id_urn . art) wad_chi
>
>  -- Record decrease of ilk total issuance
>   decrease (ilks . ix id_ilk . rum) wad_chi
>
>  -- Take custody of stablecoin from CDP owner
>   transfer DAI wad_dai id_lad Jar
>
>  -- Destroy stablecoin and anticoin
>   mend wad_dai

<p>An CDP owner can use <code>shut</code> to close their account, if
the price feed is up to date and the CDP is not in liquidation.
This reclaims all collateral and cancels all issuance plus fee.

> shut id_urn = do
>
>  -- Update debt unit and unprocessed fee revenue
>   id_ilk <- look (urns . ix id_urn . ilk)
>   chi1 <- drip id_ilk
>
>  -- Reverse all issued stablecoin plus fee
>   art0 <- look (urns . ix id_urn . art)
>   wipe id_urn (art0 * cast chi1)
>
>  -- Reclaim all locked collateral
>   ink0 <- look (urns . ix id_urn . ink)
>   free id_urn ink0
>
>  -- Nullify CDP record
>   assign (urns . at id_urn) Nothing

<h2>Assessment</h2>

<p>We define six stages of a CDP's lifecycle.

> data Stage
>
>     -- Overcollateralized, CDP type below debt ceiling, fresh price tag, liquidation not triggered
>   = Pride
>
>     -- Debt ceiling reached for CDP's type
>   | Anger
>
>     -- CDP type's collateral price feed in limbo
>   | Worry
>
>     -- CDP undercollateralized, or CDP type's price limbo grace period exceeded
>   | Panic
>
>     -- Liquidation triggered
>   | Grief
>
>     -- Liquidation triggered and started
>   | Dread
>
>   deriving (Eq, Show)

<!--

> deriving instance Generic Stage

-->

<h3>Lifecycle stage effects</h3>

<p>The following table shows which CDP actions are allowed and
prohibited in each stage of the CDP lifecycle.</p>

<figure><div>
                        decrease collateral
                           ╭┈┈┈┈┈┈┈┈┈╮
        <code>give shut lock wipe free draw bite grab plop</code>
<code>Pride</code>    ■    ■    ■    ■    ■    ■
<code>Anger</code>    ■    ■    ■    ■    ■
<code>Worry</code>    ■    ■    ■    ■
<code>Panic</code>    ■    ■    ■    ■              ■
<code>Grief</code>    ■                                  ■
<code>Dread</code>    ■                                       ■
        <code>give shut lock wipe free draw bite grab plop</code>
             ╰┈┈┈┈┈┈┈┈┈┈┈┈┈╯           ╰┈┈┈┈┈┈┈┈┈┈┈┈┈╯
           increase collateral          liquidation
</div></figure>

Some implications:

<ul>

<li>Collateral-increasing actions are allowed until <code>Grief</code>.

<li>To <code>draw</code> is only allowed during <code>Pride</code>, while
<code>free</code> is also allowed during <code>Anger</code>.

<li>To <code>give</code> is allowed at any time, including
during liquidation.

<li>Each of the liquidation actions corresponds to its own stage.

</ul>

<h3>CDP stage analysis</h3>

<p>We define the function <code>analyze</code> that determines the
lifecycle stage of a CDP.

> analyze era0 par0 urn0 ilk0 tag0 =
>
>   let
>
>    -- Value of urn's locked collateral in SDR:
>     pro = view ink urn0 * view tag tag0
>
>    -- CDP's issuance denominated in SDR:
>     con = view art urn0 * cast (view chi ilk0) * par0
>
>    -- Required collateral value as per liquidation ratio:
>     min = con * cast (view mat ilk0)
>
>    -- CDP type's total DAI issuance:
>     cap = view rum ilk0 * cast (view chi ilk0)
>
>   in if
>
>     -- Cases checked in order:
>     | has cat urn0 && view ink urn0 == 0    -> Dread
>     | has cat urn0                          -> Grief
>     | pro < min                             -> Panic
>     | view zzz tag0 + view lax ilk0 < era0  -> Panic
>     | view zzz tag0 < era0                  -> Worry
>     | cap > view hat ilk0                   -> Anger
>     | otherwise                             -> Pride

<!--
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
\meh & allowed for owner if above liquidation ratio \\
\hey & allowed for settler contract \\
\end{tabular}
}
\end{table}
-->

<p>Now we define the internal act <code>feel</code> which returns the
value of <code>analyze</code> after ensuring that the system state
is updated.

> feel id_urn = do
>
>  -- Adjust target price and target rate
>   prod
>
>  -- Update debt unit and unprocessed fee revenue
>   id_ilk <- look (urns . ix id_urn . ilk)
>   drip id_ilk
>
>  -- Read parameters for stage analysis
>   era0 <- use era
>   par0 <- use (vox . par)
>   urn0 <- look (urns . ix id_urn)
>   ilk0 <- look (ilks . ix (view ilk urn0))
>   tag0 <- look (tags . ix (view gem ilk0))
>
>  -- Return lifecycle stage of CDP
>   return (analyze era0 par0 urn0 ilk0 tag0)

<p>CDP actions use <code>feel</code> to prohibit increasing risk when
already risky, and to freeze stablecoin and collateral during
liquidation.

<h2>Adjustment</h2>

<p>The feedback mechanism is updated through <code>prod</code>, which
can be invoked at any time by keepers, but is also invoked as a side
effect of any CDP act that uses <code>feel</code> to assess risk.

> prod = do
>
>  -- Read all parameters relevant for feedback mechanism
>   era0 <- use era
>   tau0 <- use (vox . tau)
>   wut0 <- use (vox . wut)
>   par0 <- use (vox . par)
>   how0 <- use (vox . how)
>   way0 <- use (vox . way)
>
>   let
>    -- Time difference in seconds
>     age  = era0 - tau0
>
>    -- Current target rate applied to target price
>     par1  = par0 * cast (way0 ^^ age)
>
>    -- Sensitivity parameter applied over time
>     wag  = how0 * fromIntegral age
>
>    -- Target rate scaled up or down
>     way1  = inj (prj way0 +
>                  if wut0 < par0 then wag else -wag)
>
>  -- Update target price
>   assign (vox . par) par1
>
>  -- Update rate of price change
>   assign (vox . way) way1
>
>  -- Record time of update
>   assign (vox . tau) era0
>
>   where
>    -- Convert between multiplicative and additive form
>     prj x  = if x >= 1  then x - 1  else 1 - 1 / x
>     inj x  = if x >= 0  then x + 1  else 1 / (1 - x)

<p>The stability fee of an ilk can change through governance.  Due to
the constraint that acts should run in constant time, the system
cannot iterate over CDPs to effect such changes.  Instead each ilk has
a single &raquo;debt unit&laquo; which accumulates the stability fee.
The <code>drip</code> act updates this unit.  It can be called at any
time by keepers, but is also called as a side effect of every act that
uses <code>feel</code> to assess CDP risk.

> drip id_ilk = do
>
>   rho0  <- look (ilks . ix id_ilk . rho)
>   tax0  <- look (ilks . ix id_ilk . tax)
>   chi0  <- look (ilks . ix id_ilk . chi)
>   rum0  <- look (ilks . ix id_ilk . rum)
>   era0  <- use era
>
>   let
>    -- Time difference in seconds
>     age   = era0 - rho0
>    -- Value of debt unit increased according to stability fee
>     chi1  = chi0 * tax0 ^^ age
>    -- Stability fee revenue denominated in new unit
>     dew   = (cast (chi1 - chi0) :: Wad) * rum0
>
>  -- Mint stablecoin and anticoin for marginally accrued fee
>   lend dew
>
>  -- Record time of update
>   assign (ilks . ix id_ilk . rho) era0
>
>  -- Record new debt unit
>   assign (ilks . ix id_ilk . chi) chi1
>
>  -- Return the new debt unit
>   return chi1

<h2>Price feed input</h2>

<p>The <code>mark</code> act records a new market price of an
collateral along with the expiration date of this price.

> mark id_gem tag1 zzz1 = auth $ do
>   initialize (tags . at id_gem) Tag {
>     _tag  = tag1,
>     _zzz  = zzz1
>   }

<p>The <code>tell</code> act records a new market price of dai along
with the expiration date of this price.

> tell wad = auth $ do
>   assign (vox . wut) wad

<h2>Liquidation</h2>

<p>
When a CDP's stage marks it
  as in need of liquidation,
 any account can invoke the <code>bite</code> act
  to trigger the liquidation process.
This enables the settler contract
 to grab the collateral for auctioning
 and take over the anticoin.

> bite id_urn = do
>
>  -- Fail if CDP is not in the appropriate stage
>   want (feel id_urn) (== Panic)
>
>  -- Record the sender as the liquidation initiator
>   id_cat <- use sender
>   assign (urns . ix id_urn . cat) (Just id_cat)
>
>  -- Apply liquidation penalty to CDP issuance
>   id_ilk <- look (urns . ix id_urn . ilk)
>   axe0 <- look (ilks . ix id_ilk . axe)
>   art0 <- look (urns . ix id_urn . art)
>   let art1 = art0 * cast axe0
>
>  -- Update CDP issuance to include penalty
>   assign (urns . ix id_urn . art) art1

<p>
After liquidation has been triggered,
the designated settler contract invokes <code>grab</code>
to receive both the CDP's collateral
and the anticoins
corresponding to the CDP's issuance.

> grab id_urn = auth $ do
>
>  -- Fail if CDP is not marked for liquidation
>   want (feel id_urn) (== Grief)
>
>   ink0 <- look (urns . ix id_urn . ink)
>   art0 <- look (urns . ix id_urn . art)
>   id_ilk <- look (urns . ix id_urn . ilk)
>   id_tag <- look (ilks . ix id_ilk . gem)
>
>  -- Update the debt unit and unprocessed fee revenue
>   chi1 <- drip id_ilk
>
>  -- Denominate the issuance in dai
>   let con = art0 * cast chi1
>
>  -- Transfer collateral and anticoin to settler
>   transfer (Gem id_tag) ink0 Jar Vow
>   transfer SIN con Jug Vow
>
>  -- Nullify CDP's collateral and anticoin quantities
>   assign (urns . ix id_urn . ink) 0
>   assign (urns . ix id_urn . art) 0
>
>  -- Decrease the ilk's total issuance
>   decrease (ilks . ix id_ilk . rum) art0

<p>When the settler has finished the liquidation of a CDP, it
invokes <code>plop</code> to give back any collateral it did not need
to sell and restore the CDP.

> plop id_urn wad_dai = auth $ do
>
>  -- Fail unless CDP is in the proper stage
>   want (feel id_urn) (== Dread)
>
>  -- Forget the CDP's initiator of liquidation
>   assign (urns . ix id_urn . cat) Nothing
>
>  -- Take excess collateral from settler to vault
>   id_vow <- use sender
>   id_ilk <- look (urns . ix id_urn . ilk)
>   id_tag <- look (ilks . ix id_ilk . gem)
>   transfer (Gem id_tag) wad_dai id_vow Jar
>
>  -- Record the excess collateral as belonging to the CDP
>   assign (urns . ix id_urn . ink) wad_dai

<p>
The settler can invoke <code>loot</code> at any time
to claim all uncollected stability fee revenue
for use in the countercoin buy-and-burn auction.

> loot = auth $ do
>
>  -- The dai vault's balance is the uncollected stability fee revenue
>   wad <- look (balance DAI Jug)
>
>  -- Transfer the entire dai vault balance to sender
>   transfer DAI wad Jug Vow

<h2>Auctioning</h2>

<p><aside>Note: this section is incomplete; all auctions are
dummies.</aside>

> flip id_gem wad_jam wad_tab id_urn = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

> flap = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()
>
> flop = do
>   vow <- look mode
>   case vow of
>     Dummy -> return ()

<h2>Settlement</h2>

> tidy who = auth $ do
>
>  -- Find the entity's stablecoin and anticoin balances
>   awe <- look (balance DAI who)
>   woe <- look (balance SIN who)
>
>  -- We can burn at most the smallest of the two balances
>   let x = min awe woe
>
>  -- Transfer stablecoin and anticoin to the settler
>   transfer DAI x who Vow
>   transfer SIN x who Vow
>
>  -- Burn both stablecoin and anticoin
>   burn DAI x Vow
>   burn SIN x Vow

> kick = do
>
>  -- Transfer unprocessed stability fee revenue to vow account
>   loot
>
>  -- Cancel stablecoin against anticoin
>   tidy Vow
>
>  -- Assign any remaining stablecoin to countercoin-deflating auction
>   transferAll DAI Vow Flapper
>   flap
>
>  -- Assign any remaining anticoin to countercoin-inflating auction
>   transferAll SIN Vow Flopper
>   flop

<h2>Governance</h2>

<p>Governance uses <code>form</code> to create a new ilk.  Since the
new type is initialized with a zero ceiling, a separate transaction
can safely set the risk parameters before any issuance occurs.

> form id_ilk id_gem = auth $ do
>   initialize (ilks . at id_ilk) (defaultIlk id_gem)

<p>Governance uses <code>frob</code> to alter the sensitivity factor,
which is the only mutable parameter of the feedback mechanism.

> frob how1 = auth $ do
>   assign (vox . how) how1

<p>Governance can alter the five risk parameters of an ilk using
<code>cuff</code> for the liquidation ratio; <code>chop</code> for the
liquidation penalty; <code>cork</code> for the ilk ceiling;
<code>calm</code> for the duration of price limbo; and
<code>crop</code> for the stability fee.

> cuff id_ilk mat1 = auth $ do
>   assign (ilks . ix id_ilk . mat) mat1
>
> chop id_ilk axe1 = auth $ do
>   assign (ilks . ix id_ilk . axe) axe1
>
> cork id_ilk hat1 = auth $ do
>   assign (ilks . ix id_ilk . hat) hat1
>
> calm id_ilk lax1 = auth $ do
>   assign (ilks . ix id_ilk . lax) lax1

<p>When altering the stability fee with <code>crop</code>, we ensure
that the previous stability fee has been accounted for in the internal
debt unit.

> crop id_ilk tax1 = auth $ do
>
>  -- Apply the current stability fee to the internal debt unit
>   drip id_ilk
>
>  -- Change the stability fee
>   assign (ilks . ix id_ilk . tax) tax1

<h2>Token manipulation</h2>

<p>We model the ERC20 transfer function in simplified form (omitting
the concept of &raquo;allowance&laquo;).

> transfer id_gem wad src dst =
>
>  -- Operate in the token's balance table
>   zoom balances $ do
>
>  -- Fail if source balance insufficient
>   balance <- look (ix (src, id_gem))
>   aver (balance >= wad)
>
>  -- Update balances
>   decrease    (ix (src, id_gem)) wad
>   initialize  (at (dst, id_gem)) 0
>   increase    (ix (dst, id_gem)) wad

> transferAll id_gem src dst = do
>   wad <- look (balance id_gem src)
>   transfer id_gem wad src dst

<p>The internal act <code>mint</code> inflates the supply of a token.
It is used by <code>lend</code> to create new stablecoin and anticoin,
and by the settler to create new countercoin.

> mint id_gem wad dst = do
>   initialize (balances . at (dst, id_gem)) 0
>   increase   (balances . ix (dst, id_gem)) wad

<p>The internal act <code>burn</code> deflates the supply of a token.
It is used by <code>mend</code> to destroy stablecoin and anticoin,
and by the settler to destroy countercoin.

> burn id_gem wad src =
>   decrease (balances . ix (src, id_gem)) wad

<p>The internal act <code>lend</code> mints identical amounts of both
stablecoin and anticoin.  It is used by <code>draw</code> to issue
stablecoin; it is also used by <code>drip</code> to issue stablecoin
representing revenue from stability fees, which stays in the vault
until collected.

> lend wad_dai = do
>   mint DAI wad_dai Jug
>   mint SIN wad_dai Jug

<p>The internal act <code>mend</code> destroys identical amounts of
both dai and the internal debt token.  Its use via <code>wipe</code>
is how the stablecoin supply is reduced.

> mend wad_dai = do
>   burn DAI wad_dai Jug
>   burn SIN wad_dai Jug

<h1>Default data</h1>

> defaultIlk :: Id Tag -> Ilk
> defaultIlk id_tag = Ilk {
>   _gem = id_tag,
>   _axe = Ray 1,
>   _mat = Ray 1,
>   _tax = Ray 1,
>   _hat = Wad 0,
>   _lax = Sec 0,
>   _chi = Ray 1,
>   _rum = Wad 0,
>   _rho = Sec 0
> }

> emptyUrn :: Id Ilk -> Address -> Urn
> emptyUrn id_ilk id_lad = Urn {
>   _cat = Nothing,
>   _lad = id_lad,
>   _ilk = id_ilk,
>   _art = Wad 0,
>   _ink = Wad 0
> }

> initialTag :: Tag
> initialTag = Tag {
>   _tag = Wad 0,
>   _zzz = 0
> }

> initialSystem :: Ray -> System
> initialSystem how0 = System {
>   _balances = empty,
>   _ilks     = empty,
>   _urns     = empty,
>   _tags     = empty,
>   _era      = 0,
>   _sender   = God,
>   _accounts = mempty,
>   _mode     = Dummy,
>   _vox      = Vox {
>     _tau = 0,
>     _wut = Wad 1,
>     _par = Wad 1,
>     _how = how0,
>     _way = Ray 1
>   }
> }

<!--

\section{Manipulation}

> warp t = auth (do increase era t)

> mine id_gem = do
>
>     initialize (balances . at (Toy, id_gem)) 1000000000000

> hand dst wad_gem id_gem = do
>   transfer id_gem wad_gem
>     Toy (Account dst)

> sire lad = do prepend accounts lad

\section{Other stuff}

> perform :: Act -> Action ()
> perform x =
>   let ?act = x in case x of
>     Form id gem      -> form id gem
>     Mark gem tag zzz -> mark gem tag zzz
>     Open id ilk      -> open id ilk
>     Tell wad         -> tell wad
>     Frob ray         -> frob ray
>     Prod             -> prod
>     Drip x           -> drip x >>= \_ -> return ()
>     Warp t           -> warp t
>     Give urn lad     -> give urn lad
>     Lock urn wad     -> lock urn wad
>     Mine id          -> mine (Gem id)
>     Hand lad wad gem -> hand lad wad gem
>     Sire lad         -> sire lad
>     Draw lad wad     -> draw lad wad
>     Cuff ilk ray     -> cuff ilk ray
>     Chop ilk ray     -> chop ilk ray
>     Cork ilk wad     -> cork ilk wad
>     Calm ilk sec     -> calm ilk sec
>     Crop ilk ray     -> crop ilk ray
>     Mint gem wad lad -> mint gem wad lad

> being :: Actor -> Action () -> Action ()
> being who x = do
>   old <- use sender
>   assign sender who
>   y <- x
>   assign sender old
>   return y

-->

<h1>Action framework</h1>

<!--

<h2>Action descriptions</h2>

<p>We define the action vocabulary as a data type to
represent invocations.

> data Act =
>      Bite     (Id Urn)
>   |  Draw     (Id Urn)  Wad
>   |  Form     (Id Ilk)  (Id Tag)
>   |  Free     (Id Urn)  Wad
>   |  Frob     Ray
>   |  Give     (Id Urn)  Address
>   |  Grab     (Id Urn)
>   |  Lock     (Id Urn)  Wad
>   |  Loot     Wad
>   |  Mark     (Id Tag)  Wad  Sec
>   |  Open     (Id Urn)  (Id Ilk)
>   |  Prod
>   |  Shut     (Id Urn)
>   |  Tell     Wad
>   |  Wipe     (Id Urn)  Wad
>   |  Mine (Id Tag)
>   |  Hand Address Wad Token
>   |  Sire Address
>   |  Addr Address
>   |  Warp Sec
>   |  Cuff (Id Ilk) Ray
>   |  Chop (Id Ilk) Ray
>   |  Cork (Id Ilk) Wad
>   |  Calm (Id Ilk) Sec
>   |  Crop (Id Ilk) Ray
>   |  Mint Token Wad Actor
>   |  Drip (Id Ilk)
>  deriving (Eq, Show)

-->

<p>The reader does not need any abstract understanding of monads to
understand the code.  They provide syntax (the <code>do</code>
notation) for expressing exceptions and state in a way that is still
purely functional.  Each line of such a block is interpreted by the
monad to provide the semantics we want.

<h2>The <code>Action</code> monad</h2>

<p>This defines the <code>Action</code> monad as a simple composition
of a state monad and an error monad:

> type Action a = StateT System (Except Error) a

<p>We divide act failure modes into general assertion failures and
authentication failures.

> data Error = AssertError Act | AuthError
>   deriving (Show, Eq)

<p>An act can be executed on a given initial system state using
<code>exec</code>.  The result is either an error or a new state.
The <code>exec</code> function can also accept a sequence of actions,
which will be interpreted as a single transaction.

> exec :: System -> Action () -> Either Error System
> exec sys m = runExcept (execStateT m sys)

<h2>Asserting</h2>

<p>We now define a set of functions that fail unless some
condition holds.

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
>
> has p x = view p x /= Nothing

<p>We define <code>owns id_urn id_lad</code> as an assertion that the
given CDP is owned by the given account.

> owns id_urn id_lad =
>   want (look (urns . ix id_urn . lad)) ((== id_lad) . Account)

<p>We define <code>auth k</code> as an act modifier that executes
<code>k</code> only if the sender is authorized.

> auth continue = do
>   s <- use sender
>   unless (s == God) (throwError AuthError)
>   continue

<!--

\chapter{Testing}

Sketches for property stuff...

> {-
> data Parameter =
>      Wut | Par | Way
>
> maintains
>   :: Eq a  => Lens' System a -> Action ()
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
>   ::  Lens' System a -> Action ()
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
> keeps :: Parameter -> Action () -> System -> Bool
> keeps Wut  = maintains (vox . wut)
> keeps Par  = maintains (vox . par)
> keeps Way  = maintains (vox . way)
> -}

Thus:

> {- foo sys0 = all (\f -> f sys0)
>   [changesOnly (  (vox . par) `also`
>                   (vox . way))
>      (perform Prod)] -}

\appendix

%include Maker/Prelude.lhs
%include Maker/Decimal.lhs
-->

<h1>Glossary</h1>

<glossary>
<dl>

<dt><code>Id</code>
<dd><code>Id a</code> (for any <code>a</code>) is a string identifier
that cannot be mixed up with some other <code>Id b</code>.

<dt><code>Urn</code>

<dd><code>Urn</code> is the data record for a CDP.

<dt><code>Ilk</code>

<dd><code>Ilk</code> is the data record for a CDP type.

<dt><code>lad</code>

<dd>The <code>lad</code> of an <code>Urn</code> (type:
<code>Actor</code>) is the account identifier of the owner of
that CDP.

<dt><code>art</code>

<dd>The <code>art</code> of an <code>Urn</code> (type:
<code>Wad</code>) is the amount of outstanding dai issued by that CDP.

<dt><code>ink</code><dd>The <code>ink</code> of an <code>Urn</code>
(type: <code>Wad</code>) is the quantity of collateral locked in the
corresponding CDP.

<dt><code>cat</code><dd>The <code>cat</code> of an <code>Urn</code> is
the actor which triggered the CDP's liquidation, if applicable.


<dt><code>gem</code><dd>The <code>gem</code> of an <code>Ilk</code> is
the collateral token used for collateral in the corresponding CDP type.

<dt><code>tax</code><dd>The <code>tax</code> of an <code>Ilk</code>
(type: <code>Ray</code>) is the stability fee imposed on CDPs of the
corresponding CDP type, expressed as a per-second fraction of the
CDP's outstanding dai.

<dt><code>lax</code><dd>The <code>lax</code> of an <code>Ilk</code>
(type: <code>Sec</code>) is the grace period for expired collateral price
tags applying to CDPs of the corresponding type.

<dt><code>hat</code><dd>The <code>hat</code> of an <code>Ilk</code>
(type: <code>Wad</code>) is the maximum total ("ceiling") dai issuance
for the corresponding CDP type.

<dt><code>rum</code><dd>The <code>rum</code> of an <code>Ilk</code>
(type: <code>Wad</code>) is the total current issuance for the
corresponding CDP type, denominated in the CDP's internal debt unit.

<dt><code>chi</code><dd>The <code>chi</code> of an <code>Ilk</code>
(type: <code>Ray</code>) is the dai valuation for the corresponding
CDP type's internal debt unit, compounding over time according to the
CDP type's stability fee.

<dt><code>mat</code><dd>The <code>mat</code> of an <code>Ilk</code>
(type: <code>Ray</code>) is the minimum required collateralization
ratio (value of collateral divided by value of issued dai) for CDPs of
the corresponding CDP type.

<dt><code>axe</code><dd>The <code>axe</code> of an <code>Ilk</code>
(type: <code>Ray</code>) is the penalty imposed on liquidated CDPs of
the corresponding CDP type, expressed as a fraction of the CDP's
outstanding dai.

<dt><code>rho</code><dd>The <code>rho</code> of an <code>Ilk</code>
(type: <code>Sec</code>) is the timestamp of its latest debt unit
adjustment.

<dt><code>tag</code><dd>The <code>tag</code> of a <code>Tag</code>
record (type: <code>Wad</code>) is the recorded market price of the
corresponding collateral token denominated in SDR.

<dt><code>zzz</code><dd>The <code>zzz</code> of a <code>Tag</code>
(type: <code>Sec</code>) is the timestamp at which the corresponding
collateral price tag will expire.


<dt><code>Pride</code>
<dd><code>Pride</code> is the risk stage of a non-risky CDP.

<dt><code>Anger</code><dd><code>Anger</code> is the <code>Stage</code>
of a CDP whose type has reached its debt ceiling, but has a fresh
price feed, is overcollateralized, and has not been triggered
for liquidation.

<dt><code>Worry</code><dd><code>Worry</code> is the <code>Stage</code>
of a CDP whose collateral price feed has expired yet is still within
the CDP type's grace period; but the CDP is still considered
overcollateralized and has not been triggered for liquidation.  (The
CDP's type may also have reached its debt ceiling.)

<dt><code>Panic</code><dd><code>Panic</code> is the <code>Stage</code>
of a CDP which is undercollateralized or whose price feed is expired
past the CDP type's grace period; but which has not yet been triggered
for liquidation.  (The CDP's type may also have reached its
debt ceiling.)

<dt><code>Grief</code><dd><code>Grief</code> is the <code>Stage</code>
of a CDP which has been triggered for liquidation.

<dt><code>Dread</code><dd><code>Dread</code> is the <code>Stage</code>
of a CDP which is undergoing liquidation.

<dt><code>has</code><dd><code>has k x</code> is true if the field
<code>k</code> of the record <code>x</code> is not
<code>Nothing</code>.

<dt><code>Wad</code>

<dd><code>Wad</code> is the type of a decimal number with 18 decimals
of precision, used for token quantities.

<dt><code>Ray</code>

<dd><code>Ray</code> is the type of a decimal number with 36 decimals
of precision, used for precise rates and ratios.

<dt><code>Sec</code>

<dd><code>Sec</code> is the type of a timestamp or duration in
whole seconds.

<dt><code>cast</code>

<dd><code>cast x</code> converts <code>x</code> to whatever numeric
type is required in the expression context, possibly losing precision.

<dt><code>Address</code>

<dd><code>Address</code> represents an arbitrary Ethereum
account address.

<dt><code>Token</code>

<dd><code>Token</code> identifies an ERC20 token used by the system:
either some <code>Gem</code> (a collateral token) or one of
<code>SIN</code>, <code>DAI</code>, or <code>MKR</code>.

<dt><code>Gem</code>

<dd><code>Gem</code> is a constructor for a <code>Token</code>
representing a collateral token.

<dt><code>DAI</code>

<dd><code>DAI</code> is the identifier of the dai stablecoin token.

<dt><code>MKR</code>

<dd><code>MKR</code> is the identifier of the MKR token (the
countercoin and governance token).

<dt><code>SIN</code>

<dd><code>SIN</code> is the identifier of the internal "anticoin"
token which is always minted and burned in the same amounts as dai,
only kept within the system as an accounting quantity.

<dt><code>wut</code>

<dd><code>wut</code> (type: <code>Wad</code>) is the feedback
mechanism's latest market price of dai, denominated in SDR.

<dt><code>par</code>

<dd><code>par</code> (type: <code>Wad</code>) is the feedback
mechanism's latest target price of dai, denominated in SDR.

<dt><code>way</code>

<dd><code>way</code> (type: <code>Ray</code>) is the current
per-second change in target price, continuously altered by the
feedback mechanism according to the sensitivity parameter.

<dt><code>how</code>

<dd><code>how</code> (type: <code>Ray</code>) is the sensitivity
parameter of the feedback mechanism, set by governance, controlling
the rate of change of the dai target price.

<dt><code>tau</code>

<dd><code>tau</code> (type: <code>Sec</code>) is the timestamp of the
latest feedback mechanism iteration.

<dt><code>Tag</code>

<dd><code>Tag</code> is the record of collateral price feed updates.
The type <code>Id Tag</code> is used to identify collateral tokens
(aka <code>Gem</code>s).

<dt><code>Vox</code>

<dd><code>Vox</code> is the record of feedback mechanism data.

<dt><code>Actor</code>

<dd><code>Actor</code> represents the identity of an entity which can
hold a token balance or perform system actions.

<dt><code>Account</code>

<dd><code>Account</code> (type: <code>Address -> Actor</code>)
constructs an <code>Actor</code> identifier denoting an external
Ethereum account.

<dt><code>Jar</code>

<dd><code>Jar</code> (type: <code>Actor</code>) identifies the
system's collateral vault.

<dt><code>Jug</code>

<dd><code>Jug</code> (type: <code>Actor</code>) identifies the
actor that mints DAI/SIN and holds SIN.

<dt><code>Vow</code>

<dd><code>Vow</code> (type: <code>Actor</code>) identifies the
system's settler component.

<dt><code>Flipper</code>

<dd><code>Flipper</code> (type: <code>Actor</code>) identifies the
collateral auctioneer component.

<dt><code>Flapper</code>

<dd><code>Flapper</code> (type: <code>Actor</code>) identifies the
DAI stablecoin auctioneer component.

<dt><code>Flopper</code>

<dd><code>Flopper</code> (type: <code>Actor</code>) identifies the MKR
countercoin auctioneer component.

<dt><code>Toy</code>

<dd><code>Toy</code> (type: <code>Actor</code>) identifies the
system's test driver (not present in production).

<dt><code>God</code>

<dd><code>God</code> (type: <code>Actor</code>) identifies an
omnipotent actor (prototyping kludge, will be removed).

<dt><code>lock</code>

<dd><code>lock</code> transfers collateral from a CDP owner to the
system's token vault and records an increase of <code>ink</code> to
the CDP's <code>Urn</code>.

<dt><code>draw</code>

<dd><code>draw</code> mints new <code>DAI</code> for the owner of an
overcollateralized CDP.

<dt><code>give</code>

<dd><code>give</code> transfers ownership of a CDP.

<dt><code>free</code>

<dd><code>free</code> reclaims collateral from an
overcollateralized CDP.

<dt><code>prod</code>

<dd><code>prod</code> updates the stability feedback mechanism.
It adjusts the target price (<code>par</code>) according to the target
rate (<code>way</code>), and adjusts the target rate according to the
current market price (<code>wut</code>) and the sensitivity parameter
(<code>how</code>).

<dt><code>feel</code>

<dd><code>feel</code> calculates the <code>Stage</code> of a CDP. This
involves deciding collateralization requirements as well as checking
price feed status and liquidation progress.

</dl>
</glossary>

<script src=maker.js></script>
