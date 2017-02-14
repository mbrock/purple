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
  Precise, E18, E36, Wad, Ray, cast, wad, ray,
  main
) where

import Control.Lens hiding (elements)

import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (lookup)
import Control.Applicative (pure)
import Control.Arrow (first)
import Control.Monad (guard)

import Data.Fixed
import Data.CReal

import Data.Map (  Map, lookup, insert, adjust, empty,
                   singleton, elems, keys, size, (!))
import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  (  property, Arbitrary (..), Gen, (==>),
     generate, sized, label, classify,
     choose, shuffle, oneof, elements )

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

instance Show Wad where
  show (Wad x) = show x

instance Show Ray where
  show (Ray x) = show x

type Precise = CReal 256

cast :: (Real a, Fractional b) => a -> b
cast = fromRational . toRational

wad  :: Real a => a -> Wad;  wad = cast
ray  :: Real a => a -> Ray;  ray = cast

epsilon :: forall a. HasResolution a => Fixed a
epsilon = 1 / fromIntegral (resolution (undefined :: Fixed a))

newtype Nat = Nat Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data Id a = Id String
  deriving (Show, Eq, Ord)

type IdMap a = Map (Id a) a

data Lad = Lad deriving (Eq, Show)

data Gem = Gem {
  gemTotalSupply  :: Wad,
  gemBalanceOf    :: Map (Id Lad)          Wad,
  gemAllowance    :: Map (Id Lad, Id Lad)  Wad
} deriving (Eq, Show)

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
} deriving (Eq, Show)

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
  systemEra   :: Int,
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
  vatFix   = Wad 0,
  vatPar   = Wad 0,
  vatHow   = Ray 0,
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
  deriving (Eq, Show)

data Env = Env
  { envLad :: Id Lad }
  deriving (Eq, Show)

makeFields ''Env

type Maker e s a = ReaderT e (StateT s Identity) a

newLad :: HasLads s (IdMap Lad)
       => Id Lad -> Maker e s ()
newLad id =
  lads . at id ?= Lad

newJar :: (HasVat s vat, HasJars vat (IdMap Jar))
       => Id Jar -> Jar -> Maker e s ()
newJar id jar =
  vat . jars . at id ?= jar 

formIlk :: (HasVat s vat, HasIlks vat (IdMap Ilk))
        => Id Ilk -> Id Jar -> Maker e s ()
formIlk id jar =
  vat . ilks . at id ?= defaultIlk jar

openUrn
  :: (HasVat s vat, HasUrns vat (IdMap Urn), HasLad e (Id Lad))
  => Id Urn -> Id Ilk -> Maker e s ()
openUrn id ilk = do
  theLad <- view lad
  vat . urns . at id ?= defaultUrn ilk theLad

perform :: Action -> Maker Env System ()
perform = \case
  NewLad id      -> newLad id
  NewJar id jar  -> newJar id jar
  FormIlk id jar -> formIlk id jar
  OpenUrn id ilk -> openUrn id ilk

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

instance Arbitrary Ray where
  arbitrary = ray <$> choose (0, 2 :: Precise)

instance Arbitrary (Id a) where
  arbitrary = Id <$> shuffle ['a'..'m']

instance Arbitrary Nat where
  arbitrary = Nat <$> choose (0, 1000)

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
  if       size (sys ^. lads) ==  0        then newLad
  else if  size (sys ^. vat . jars) <   2  then newJar
  else if  size (sys ^. vat . ilks) <   1  then formIlk
  else oneof [newLad, newJar, formIlk, openUrn]
  where
    newLad = (,) <$> pure (Id "God") <*> (NewLad <$> arbitrary)
    newJar = do
      (id, tag, zzz) <- arbitrary
      lad  <- elements (keys (sys ^. lads))
      gem  <- monopolizedGem <$> arbitrary <*> pure lad
      return (Id "God", NewJar id (Jar gem tag zzz))
    formIlk = do
      id   <- arbitrary
      jar  <- elements (keys (sys ^. vat . jars))
      return (Id "God", FormIlk id jar)
    openUrn = do
      id   <- arbitrary
      ilk  <- elements (keys (sys ^. vat . ilks))
      lad  <- elements (keys (sys ^. lads))
      return (lad, OpenUrn id ilk)

instance Arbitrary System where
  arbitrary = sized (\n -> f n initialSystem)
    where
      f 0 sys = return sys
      f n sys = do
        (ladId, a) <- marginalAction sys
        f (n - 1) (execState (runReaderT (perform a) Env { envLad = ladId }) sys)

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
      let  wad   = Wad epsilon + gemBalanceOf gem ! src
           gem'  = transferFrom src dst wad gem
      in gem' == Nothing,

  testProperty "no overtransfer" $
    \(GemWithCouple src dst gem) ->
      let  wad   = Wad epsilon + gemBalanceOf gem ! src
           gem'  = transferFrom src dst wad gem
      in gem' == Nothing
 ]

main :: IO ()
main = defaultMain $
  testGroup "Maker" [
    decimalFixedPointProperties,
    gemProperties
  ]
