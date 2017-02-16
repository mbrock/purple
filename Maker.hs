{-# Language DataKinds #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Maker (
  Precise, E18, E36, Wad, Ray, Id (..), cast, wad, ray,
  Action, perform, exec, initialSystem, report
) where

import Control.Lens hiding (elements)

import Control.Monad.State hiding (fix)
import Control.Monad.Reader hiding (fix)

import Prelude hiding (lookup)
import Control.Applicative (pure)
import Control.Arrow (first)
import Control.Monad (guard)
import Data.Foldable (foldl')

import Data.Fixed
import Data.CReal

import Data.Map (  Map, lookup, insert, adjust, empty,
                   singleton, elems, keys, size, (!))
import qualified Data.Map as Map

import Debug.Trace

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  (  Property, property, Arbitrary (..), Gen, (==>),
     generate, sized, label, classify, cover,
     choose, shuffle, oneof, elements, collect,
     frequency, conjoin, counterexample, (===)
  )

newtype Wad = Wad (Fixed E18)
  deriving (  Ord, Eq, Num, Real, Fractional)

data E18 = E18
instance HasResolution E18 where
  resolution _ = 10^18

newtype Ray = Ray (Fixed E36)
  deriving (  Ord, Eq, Num, Real, Fractional)

data E36 = E36
instance HasResolution E36 where
  resolution _ = 10^36

instance Read Ray where
  readsPrec n s = first Ray <$> readsPrec n s

instance Read Wad where
  readsPrec n s = first Wad <$> readsPrec n s

instance Read Nat where
  readsPrec n s = first Nat <$> readsPrec n s

instance Show Wad where
  show (Wad x) = show x

instance Show Ray where
  show (Ray x) = show x

instance Show Nat where
  show (Nat x) = show x

type Precise = CReal 256

cast :: (Real a, Fractional b) => a -> b
cast = fromRational . toRational

wad  :: Real a => a -> Wad;  wad = cast
ray  :: Real a => a -> Ray;  ray = cast

class Epsilon e where
  epsilon :: e

instance HasResolution a => Epsilon (Fixed a) where
  epsilon = 1 / fromIntegral (resolution (undefined :: Fixed a))

instance Epsilon Wad where
  epsilon = Wad epsilon

instance Epsilon Ray where
  epsilon = Ray epsilon

newtype Nat = Nat Int
  deriving (Eq, Ord, Enum, Num, Real, Integral)

data Id a = Id String
  deriving (Show, Eq, Ord)

instance Read (Id a) where
  readsPrec n s = first Id <$> readsPrec n s

type IdMap a = Map (Id a) a

data Lad = Lad deriving (Eq, Show)

data Gem = Gem {
  gemTotalSupply  :: Wad,
  gemBalanceOf    :: Map (Id Lad)          Wad,
  gemAllowance    :: Map (Id Lad, Id Lad)  Wad
} deriving (Eq, Show, Read)

makeFields ''Gem

monopolizedGem :: Wad -> Id Lad -> Gem
monopolizedGem gemTotalSupply croesus =
  let  gemBalanceOf  = singleton croesus gemTotalSupply
       gemAllowance  = empty
  in Gem {..}

data Jar = Jar {
  jarGem  :: Gem,          -- ERC20 token
  jarTag  :: Wad,          -- Market price
  jarZzz  :: Nat           -- Price expiration
} deriving (Eq, Show, Read)

makeFields ''Jar

data Ilk = Ilk {
  ilkJar  :: Id Jar,       -- Collateral vault
  ilkAxe  :: Ray,          -- Liquidation penalty
  ilkMat  :: Ray,          -- Liquidation ratio
  ilkTax  :: Ray,          -- Stability fee
  ilkHat  :: Wad,          -- Debt ceiling
  ilkLag  :: Nat,          -- Limbo duration
  ilkBag  :: Map Nat Ray,  -- Accumulator
  ilkRho  :: Nat           -- Last poked
} deriving (Eq, Show)

makeFields ''Ilk

data Urn = Urn {
  urnIlk  :: Id Ilk,       -- CDP type
  urnLad  :: Id Lad,       -- Issuer
  urnPro  :: Wad,          -- Collateral amount
  urnCon  :: Wad,          -- Outstanding dai debt
  urnPhi  :: Nat           -- Last poked
} deriving (Eq, Show)

makeFields ''Urn

data Vat = Vat {
  vatTau  :: Nat,         -- Last poked
  vatFix  :: Wad,         -- Market price
  vatPar  :: Wad,         -- Target price
  vatHow  :: Ray,         -- Sensitivity
  vatWay  :: Ray,         -- Target rate
  vatPie  :: Wad,         -- Unprocessed fees
  vatSin  :: Wad,         -- Bad debt
  vatJars  :: IdMap Jar,  -- ERC20 tokens
  vatIlks  :: IdMap Ilk,  -- CDP types
  vatUrns  :: IdMap Urn   -- CDPs
} deriving (Eq, Show)

makeFields ''Vat

data System = System {
  systemVat   :: Vat,
  systemEra   :: Nat,
  systemLads  :: IdMap Lad   -- System users
} deriving (Eq, Show)

makeFields ''System

defaultIlk :: Id Jar -> Ilk
defaultIlk jarId = Ilk {
  ilkJar  = jarId,
  ilkAxe  = Ray 1,
  ilkMat  = Ray 1,
  ilkTax  = Ray 1,
  ilkHat  = Wad 0,
  ilkLag  = Nat 0,
  ilkBag  = empty,
  ilkRho  = Nat 0
}

defaultUrn :: Id Ilk -> Id Lad -> Urn
defaultUrn ilk lad = Urn {
  urnIlk  = ilk,
  urnLad  = lad,
  urnPro  = Wad 0,
  urnCon  = Wad 0,
  urnPhi  = Nat 0
}

initialVat :: Vat
initialVat = Vat {
  vatTau   = 0,
  vatFix   = Wad 1,
  vatPar   = Wad 1,
  vatHow   = Ray (1 / 3600 / 24),
  vatWay   = Ray 1,
  vatPie   = Wad 0,
  vatSin   = Wad 0,
  vatIlks  = empty,
  vatUrns  = empty,
  vatJars  = singleton (Id "DAI") Jar {
    jarGem  = Gem {
      gemTotalSupply  = 0,
      gemBalanceOf    = empty,
      gemAllowance    = empty
    },
    jarTag  = Wad 0,
    jarZzz  = 0
  }
}

initialSystem :: System
initialSystem = System {
  systemVat   = initialVat,
  systemLads  = empty,
  systemEra   = 0
}

data Action =
     NewLad   (Id Lad)
  |  NewJar   (Id Jar)  Jar
  |  FormIlk  (Id Ilk)  (Id Jar)
  |  OpenUrn  (Id Urn)  (Id Ilk)
  |  Tell     Wad
  |  Frob     Ray
  |  Poke
  |  Warp     Nat
  deriving (Eq, Show, Read)

data Env = Env
  { envLad :: Id Lad
  , envSys :: System }
  deriving (Eq, Show)

makeFields ''Env

type Maker e s a = ReaderT e (StateT s Identity) a

newLad
  :: HasLads s (IdMap Lad)
  => Id Lad -> Maker e s ()
newLad id =
  lads.at id ?= Lad

newJar
  :: (HasVat s vat,
      HasJars vat (IdMap Jar))
  => Id Jar -> Jar -> Maker e s ()
newJar id jar =
  vat.jars.at id ?= jar 

formIlk
  :: (HasVat s vat,
      HasIlks vat (IdMap Ilk))
  => Id Ilk -> Id Jar -> Maker e s ()
formIlk id jar =
  vat.ilks.at id ?= defaultIlk jar

openUrn
  :: (HasVat s vat,
      HasUrns vat (IdMap Urn),
      HasLad e (Id Lad))
  => Id Urn -> Id Ilk -> Maker e s ()
openUrn id ilk = do
  theLad <- view lad
  vat.urns.at id ?= defaultUrn ilk theLad

tell
  :: (HasVat write vat',
      HasFix vat' Wad)
  => Wad -> Maker e write ()
tell x =
  vat.fix .= x

frob
  :: (HasVat write vat',
      HasHow vat' Ray)
  => Ray -> Maker e write ()
frob x =
  vat.how .= x

poke
  :: (HasSys read sys,
      HasVat sys vat,
      HasEra sys Nat,
      HasTau vat Nat,
      HasHow vat Ray,
      HasWay vat Ray,
      HasFix vat Wad,
      HasVat write vat',
      HasPar vat' Wad,
      HasWay vat' Ray,
      HasTau vat' Nat)
  => Maker read write ()
poke = do
  theEra <- view $ sys.era
  theFix <- view $ sys.vat.fix
  theHow <- view $ sys.vat.how
  oldTau <- view $ sys.vat.tau
  oldWay <- view $ sys.vat.way
  oldPar <- use  $ vat.par
  
  let fan = theEra - oldTau
      wag = theHow * fromIntegral fan
      
  let newPar = oldPar * cast (oldWay ^^ fan)
      newWay = inj (prj oldWay +
                    if theFix < newPar
                    then wag
                    else -wag)

  vat.par .= newPar
  vat.way .= newWay
  vat.tau .= theEra

  where
    prj x = if x >= 1 then x - 1 else 1 - 1 / x
    inj x = if x >= 0 then x + 1 else 1 / (1 - x)

warp
  :: (HasEra write Nat)
  => Nat -> Maker e write ()
warp t =
  era .= t

say = putStrLn

report :: System -> IO ()
report sys = do
  say $ "era\t" ++ show (sys^.era)
  say $ "tau\t" ++ show (sys^.vat.tau)
  say $ "how\t" ++ show (sys^.vat.how)
  say $ "way\t" ++ show (sys^.vat.way)
  say $ "fix\t" ++ show (sys^.vat.fix)
  say $ "par\t" ++ show (sys^.vat.par)

perform :: Action -> Maker Env System ()
perform x = case x of
  NewLad id      -> newLad id
  NewJar id jar  -> newJar id jar
  FormIlk id jar -> formIlk id jar
  OpenUrn id ilk -> openUrn id ilk
  Tell wad       -> tell wad
  Frob ray       -> frob ray
  Poke           -> poke
  Warp t         -> warp t

scenario :: System -> [Action] -> System
scenario s xs = foldl' (exec (Id "God")) s (map perform xs)

transferFrom  ::  Id Lad -> Id Lad -> Wad
              ->  Gem -> Maybe Gem
transferFrom src dst wad gem =
  do  balance <- gem ^. balanceOf . at src
      guard (balance >= wad)
      return $
        gem & balanceOf . at src . _Just -~ wad
            & balanceOf . at dst %~
                (\case Nothing -> Just wad
                       Just x -> Just (wad + x))

instance Arbitrary Wad where
  arbitrary = wad <$> choose (0, 10000 :: Precise)
  shrink (Wad x) = map Wad (shrink x)

instance Arbitrary Ray where
  arbitrary = ray <$> choose (0, 2 :: Precise)
  shrink (Ray x) = map Ray (shrink x)

instance Arbitrary (Id a) where
  arbitrary = Id <$> shuffle ['a'..'m']

instance Arbitrary Nat where
  arbitrary = Nat <$> choose (0, 1000)
  shrink (Nat x) = map Nat (shrink x)

instance Arbitrary Gem where
  arbitrary = do
    gemBalanceOf <- arbitrary
    let  gemTotalSupply  = sum (elems gemBalanceOf)
         gemAllowance    = mempty
    return Gem { .. }

data GemWithCouple =
  GemWithCouple (Id Lad) (Id Lad) Gem
  deriving (Show, Eq)

instance Arbitrary GemWithCouple where
  arbitrary = do
    gem                             <- arbitrary
    ((bob, bobWad), (eve, eveWad))  <- arbitrary
    return $ GemWithCouple bob eve gem {
      gemBalanceOf =
        insert bob bobWad $
          insert eve eveWad (gemBalanceOf gem)
   }

marginalAction :: System -> Gen (Id Lad, Action)
marginalAction sys =
  if       size (sys ^. lads) ==  0      then newLad
  else if  size (sys ^. vat.jars) <   2  then newJar
  else if  size (sys ^. vat.ilks) <   1  then formIlk
  else if  sys ^. vat.fix == sys ^. vat.par    then genTell
  else frequency [
    (3, newLad),
    (3, newJar),
    (1, formIlk),
    (2, openUrn),
    (1, genFrob),
    (5, genPoke),
    (5, genWarp)
  ]
  where
    newLad = (,) <$> pure (Id "God") <*> (NewLad <$> arbitrary)
    newJar = do
      (id, tag, zzz) <- arbitrary
      lad  <- elements (keys (sys ^. lads))
      gem  <- monopolizedGem <$> arbitrary <*> pure lad
      return (Id "God", NewJar id (Jar gem tag zzz))
    formIlk = do
      id   <- arbitrary
      jar  <- elements (keys (sys ^. vat.jars))
      return (Id "God", FormIlk id jar)
    openUrn = do
      id   <- arbitrary
      ilk  <- elements (keys (sys ^. vat.ilks))
      lad  <- elements (keys (sys ^. lads))
      return (lad, OpenUrn id ilk)
    genTell = do
      k  <- wad . (+ 1) <$> choose (-0.1, 0.1 :: Precise)
      return (Id "God", Tell (k * sys ^. vat.fix))
    genFrob = do
      x  <- ray <$> choose (1/3600/24/30, 1/3600/24 :: Precise)
      return (Id "God", Frob (x))
    genWarp = do
      dt <- choose (1, 100000)
      return (Id "God", Warp (sys ^. era + Nat dt))
    genPoke = do
      return (Id "God", Poke)

exec ladId system m =
  flip execState system
    (runReaderT m Env { envLad = ladId, envSys = system })

instance Arbitrary System where
  arbitrary = sized (\n -> f (n) initialSystem)
    where
      f 0 sys = return sys
      f n sys = do
        (ladId, a) <- marginalAction sys
        f (n - 1) $ exec ladId sys (perform a)

  shrink sys = wipeAccounts
    where
      wipeAccounts =
        if size (sys ^. lads) > 0
        then [sys & lads .~ mempty
                  & vat . urns .~ empty
                  & vat . ilks .~ empty
                  & vat . jars .~ empty
             ]
        else []

decimalFixedPointProperties =
  testGroup "Decimal fixed points" [

    testCase "show/read format" $ do
      show (Wad epsilon) @?=
        "0." ++ replicate 17 '0' ++ "1"

      show (Ray epsilon) @?=
        "0." ++ replicate 35 '0' ++ "1",

    testProperty "show/read isomorphism" $
      \x -> read (show x) == (x :: Wad),

    testCase "epsilon" $ do
      assert $ (Wad epsilon)  > 0
      assert $ (Ray epsilon)  > 0
      (Wad  epsilon  * Wad  epsilon)  @?= 0
      (Ray  epsilon  * Ray  epsilon)  @?= 0
 ]

gemProperties = testGroup "Gems" [

  testProperty "supply invariant" $
    \(GemWithCouple src dst gem) ->
      let  Just wad   = lookup src (gemBalanceOf gem)
           Just gem'  = transferFrom src dst wad gem
      in gemTotalSupply gem' == gemTotalSupply gem,

  testProperty "no overdrawing" $
    \(GemWithCouple src dst gem) ->
      let  wad   = epsilon + gemBalanceOf gem ! src
           gem'  = transferFrom src dst wad gem
      in gem' == Nothing,

  testProperty "no overtransfer" $
    \(GemWithCouple src dst gem) ->
      let  wad   = epsilon + gemBalanceOf gem ! src
           gem'  = transferFrom src dst wad gem
      in gem' == Nothing
 ]

someLad = Id "someone"

pokeProperties = testGroup "Poke"
  [ testProperty "era idempotence" $
      \sys0 ->
        let sys1 = exec someLad sys0 $ do
                     perform Poke
            sys2 = exec someLad sys1 $ do
                     perform Poke
                     perform Poke
        in sys1 == sys2
        
  , testProperty "way changes when par /= fix" $
      \sys0 ->
        (sys0 ^. vat.fix /= sys0 ^. vat.par)
        ==>
        let sys1 = sys0 & era +~ 1
            sys2 = exec someLad sys1 (perform Poke)
        in changed (vat.way) sys1 sys2
        
  , testProperty "par changes in the direction of way" $
      \(sys0, dt) ->
        let sys1 = exec someLad (sys0 & era +~ dt) (perform Poke)
        in (sys1 ^. vat.way /= 1)
           ==>
           let sys2 = sys1 & era +~ dt
               sys3 = exec someLad sys2 (perform Poke)
           in collect (compare (sys1 ^. vat.way) 1) $
             if sys1 ^. vat.way > 1
             then increased (vat.par) sys2 sys3
             else decreased (vat.par) sys2 sys3
             
  , testProperty "par changes at how rate" $
      \sys0 ->
        let sys1 = exec someLad (sys0 & era +~ 1) (perform Poke)
        in (sys1 ^. vat.way /= 1)
           ==>
           let sys2 = sys1 & era +~ 1
               sys3 = exec someLad sys2 (perform Poke)
           in collect (sys2 ^. vat.par) $ -- (compare (sys1 ^. vat.way) 1) $
             if sys1 ^. vat.way >= 1
             then
               sys3 ^. vat.par / sys2 ^. vat.par
               ~==
               wad (1 + sys1^.vat.how)
             else
               sys2 ^. vat.par / sys3 ^. vat.par
               ~==
               wad (1 + sys1^.vat.how)
  ]

kept x a b = a ^. x === b ^. x
changed x a b = a ^. x /== b ^. x 

infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)

infix 4 ~==
(~==)
  :: (Num a, Ord a, Show a, Epsilon a)
  => a -> a -> Property
x ~== y =
  counterexample (show x ++ " /~= " ++ show y)
    (abs (x - y) < 1000 * epsilon)

increased x a b =
  let ax = a ^. x
      bx = b ^. x
  in counterexample (show ax ++ " <= " ++ show bx) (bx > ax)

decreased x a b =
  let ax = a ^. x
      bx = b ^. x
  in counterexample (show ax ++ " >= " ++ show bx) (bx < ax)

main :: IO ()
main = defaultMain $
  testGroup "Maker" [
    decimalFixedPointProperties,
    gemProperties,
    pokeProperties
  ]
