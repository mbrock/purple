{-# Language DataKinds #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}

module Maker (
  Precise, E18, E36, Wad, Ray, cast, wad, ray,
  main
) where

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

class HasId a where
  data Id a
  mkId :: String -> Id a

data Gem = Gem {
  totalSupply  :: Wad,
  balanceOf    :: Map (Id Lad)          Wad,
  allowance    :: Map (Id Lad, Id Lad)  Wad
} deriving (Eq, Show)

monopolizedGem :: Wad -> Id Lad -> Gem
monopolizedGem totalSupply croesus =
  let  balanceOf  = singleton croesus totalSupply
       allowance  = empty
  in Gem {..}

data Jar = Jar {
  gem  :: Gem,          -- ERC20 token
  tag  :: Wad,          -- Market price
  zzz  :: Nat           -- Price expiration
} deriving (Eq, Show)

data Urn = Urn {
  ilk  :: Id Ilk,       -- CDP type
  lad  :: Id Lad,       -- Issuer
  pro  :: Wad,          -- Collateral amount
  con  :: Wad,          -- Outstanding dai debt
  age  :: Nat           -- Last poked
} deriving (Eq, Show)

data Ilk = Ilk {
  jar  :: Id Jar,       -- Collateral vault
  axe  :: Ray,          -- Liquidation penalty
  mat  :: Ray,          -- Liquidation ratio
  tax  :: Ray,          -- Stability fee
  hat  :: Wad,          -- Debt ceiling
  lag  :: Nat,          -- Limbo duration
  bag  :: Map Nat Ray,  -- Accumulator
  age  :: Nat           -- Last poked
} deriving (Eq, Show)

data Dao = Dao {
  age  :: Nat,                -- Last poked
  fix  :: Wad,                -- Market price
  par  :: Wad,                -- Target price
  how  :: Ray,                -- Sensitivity
  way  :: Ray,                -- Target rate
  pie  :: Wad,                -- Unprocessed fees
  sin  :: Wad,                -- Bad debt
  ilks  :: Map (Id Ilk) Ilk,  -- CDP types
  urns  :: Map (Id Urn) Urn,  -- CDPs
  jars  :: Map (Id Jar) Jar,  -- ERC20 tokens
  lads  :: Map (Id Lad) Lad   -- System users
} deriving (Eq, Show)

data Lad = Lad deriving (Eq, Show)

instance HasId Urn where
  data Id Urn = UrnId String
    deriving (Show, Eq, Ord)
  mkId = UrnId

instance HasId Ilk where
  data Id Ilk = IlkId String
    deriving (Show, Eq, Ord)
  mkId = IlkId

instance HasId Gem where
  data Id Gem = GemId String
    deriving (Show, Eq, Ord)
  mkId = GemId

instance HasId Jar where
  data Id Jar = JarId String
    deriving (Show, Eq, Ord)
  mkId = JarId

instance HasId Lad where
  data Id Lad = LadId String
    deriving (Show, Eq, Ord)
  mkId = LadId

defaultIlk :: Id Jar -> Ilk
defaultIlk jarId = Ilk {
  jar  = jarId,
  axe  = Ray 1,
  mat  = Ray 1,
  tax  = Ray 1,
  hat  = Wad 0,
  lag  = Nat 0,
  bag  = empty,
  age  = Nat 0
}

defaultUrn :: Id Ilk -> Id Lad -> Urn
defaultUrn ilk lad = Urn {
  ilk  = ilk,
  lad  = lad,
  pro  = Wad 0,
  con  = Wad 0,
  age  = Nat 0
}

initialDao :: Dao
initialDao = Dao {
  age   = 0,
  fix   = Wad 0,
  par   = Wad 0,
  how   = Ray 0,
  way   = Ray 1,
  pie   = Wad 0,
  sin   = Wad 0,
  ilks  = empty,
  urns  = empty,
  lads  = empty,
  jars  = singleton (JarId "DAI") Jar {
    gem  = Gem {
      totalSupply  = 0,
      balanceOf    = empty,
      allowance    = empty
    },
    tag  = Wad 0,
    zzz  = 0
  }
}

data Action =
     NewLad   (Id Lad)
  |  NewJar   (Id Jar)  Jar
  |  FormIlk  (Id Ilk)  (Id Jar)
  |  OpenUrn  (Id Urn)  (Id Ilk)  (Id Lad)
  deriving (Eq, Show)

perform :: Dao -> Action -> Dao
perform dao = \case
  NewLad id           -> dao {
    lads = insert id Lad (lads dao)
  }
  NewJar id jar       -> dao {
    jars = insert id jar (jars dao)
  }
  FormIlk id jar      -> dao {
    ilks = insert id (defaultIlk jar) (ilks dao)
  }
  OpenUrn id ilk lad  -> dao {
    urns = insert id (defaultUrn ilk lad) (urns dao)
  }

decrease, increase :: Ord k =>
  Wad -> k -> Map k Wad -> Map k Wad
decrease  d = adjust (\x -> x - d)
increase  d = adjust (\x -> x + d)

transferFrom  ::  Id Lad -> Id Lad -> Wad
              ->  Gem -> Maybe Gem
transferFrom src dst wad gem =
  do  balance <- lookup src (balanceOf gem)
      guard (balance >= wad)
      return gem {
        balanceOf =
          decrease wad src $
            increase wad dst (balanceOf gem)
      }

instance Arbitrary Wad where
  arbitrary = wad <$> choose (0, 10000 :: Precise)

instance Arbitrary Ray where
  arbitrary = ray <$> choose (0, 2 :: Precise)

instance HasId a => Arbitrary (Id a) where
  arbitrary = mkId <$> shuffle ['a'..'m']

instance Arbitrary Nat where
  arbitrary = Nat <$> choose (0, 1000)

instance Arbitrary Gem where
  arbitrary = do
    balanceOf <- arbitrary
    let  totalSupply  = sum (elems balanceOf)
         allowance    = mempty
    return Gem { .. }

data GemWithCouple =
  GemWithCouple (Id Lad) (Id Lad) Gem
  deriving (Show, Eq)

instance Arbitrary GemWithCouple where
  arbitrary = do
    gem                             <- arbitrary
    ((bob, bobWad), (eve, eveWad))  <- arbitrary
    return $ GemWithCouple bob eve gem {
      balanceOf =
        insert bob bobWad $
          insert eve eveWad (balanceOf gem)
   }


marginalAction :: Dao -> Gen Action
marginalAction dao =
  if       size (lads dao) ==  0  then newLad
  else if  size (jars dao) <   2  then newJar
  else if  size (ilks dao) <   1  then formIlk
  else oneof [newLad, newJar, formIlk, openUrn]
  where
    newLad = NewLad <$> arbitrary
    newJar = do
      (id, tag, zzz) <- arbitrary
      lad  <- elements (keys (lads dao))
      gem  <- monopolizedGem <$> arbitrary <*> pure lad
      return (NewJar id (Jar gem tag zzz))
    formIlk = do
      id   <- arbitrary
      jar  <- elements (keys (jars dao))
      return (FormIlk id jar)
    openUrn = do
      id   <- arbitrary
      ilk  <- elements (keys (ilks dao))
      lad  <- elements (keys (lads dao))
      return (OpenUrn id ilk lad)

instance Arbitrary Dao where
  arbitrary = sized (\n -> f n initialDao)
    where
      f 0 dao = return dao
      f n dao = do
        a <- marginalAction dao
        f (n - 1) (perform dao a)

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
      let  Just wad   = lookup src (balanceOf gem)
           Just gem'  = transferFrom src dst wad gem
      in totalSupply gem' == totalSupply gem,

  testProperty "no overdrawing" $
    \(GemWithCouple src dst gem) ->
      let  wad   = Wad epsilon + balanceOf gem ! src
           gem'  = transferFrom src dst wad gem
      in gem' == Nothing,

  testProperty "no overtransfer" $
    \(GemWithCouple src dst gem) ->
      let  wad   = Wad epsilon + balanceOf gem ! src
           gem'  = transferFrom src dst wad gem
      in gem' == Nothing
 ]

main :: IO ()
main = defaultMain $
  testGroup "MakerDao900" [
    decimalFixedPointProperties,
    gemProperties
  ]
