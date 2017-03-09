{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImplicitParams #-}
{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Main where

import Prelude (IO, print, snd, round)
import IPPrint.Colored

import Maker
import Maker.Prelude

import qualified Data.Map as Map

import Control.Arrow ((>>>))
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad (sequence_, forM_)

import Data.CReal
import Data.Text (Text, pack, intercalate)
import Data.Text.IO (putStrLn)
import Data.Foldable (toList, foldl')

import qualified Data.Sequence as Seq

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  (  Property, property, Arbitrary (..), Gen, (==>),
     generate, sized, label, classify, cover,
     choose, shuffle, oneof, elements, collect,
     frequency, conjoin, counterexample, (===)
  )

type Precise = CReal 256

wad :: Precise -> Wad
wad = cast

ray :: Precise -> Ray
ray = cast

data Comparison a = Exactly a
  deriving Show

data Claim =
     WadIs Text (Comparison Wad)
  |  RayIs Text (Comparison Ray)
  |  SecIs Text (Comparison Sec)
  deriving Show

solidityClaim =
  let assertDecimal n x (Exactly y) =
        "assertEqDecimal(" <>
          "uint256(" <> x <> "), " <>
          "uint256(" <> txt (round (y * (10^^n))) <> "), " <>
          pack (show (n :: Int)) <> "); // " <> txt y

  in \case
    WadIs e x -> assertDecimal 18 e x
    RayIs e x -> assertDecimal 36 e x
    SecIs e (Exactly x) ->
      "assertEq(uint(" <> e <> "), uint(" <> txt x <> "));"

txt :: Show a => a -> Text
txt = pack . show

unwad x = round $ x * 1000000000000000000
unray x = round $ x * 1000000000000000000000000000000000000
unsec x = x

solidityAct :: (Address, Act) -> Text
solidityAct = \case
  (_, Prod)    ->
    "this.prod();"
  (_, Frob x)  ->
    "this.frob(" <> txt (unray x) <> "); // " <> txt x
  (_, Tell x)  ->
    "this.tell(" <> txt (unwad x) <> "); // " <> txt x
  (_, Warp x)  ->
    "this.warp(" <> txt (unsec x) <> ");"
  (_, Form (Id x) (Id y)) ->
    "this.form(" <> txt x <> ", " <> txt y <> ");"
  (_, Mine (Id x))   ->
    "this.gem(" <> txt x <> ");"
  (_, Sire (Address x))   ->
    "this.lad(" <> txt x <> ");"
  (_, Hand (Address x) y (Id z))   ->
    "this.hand(" <> txt x <> ", " <> txt (unwad y) <> ", " <> txt z<> ");"
  (Address x, Open (Id _) (Id z)) ->
    "this.open(" <> txt x <> ", " <> txt z <> ");"
  (Address x, Lock (Id _) z) ->
    "this.lock(" <> txt x <> ", " <> txt (unwad z) <> "); // " <> txt z

actsCode :: Seq (Address, Act) -> Seq Text
actsCode = fmap solidityAct

indent :: Seq Text -> Seq Text
indent = fmap ("  " <>)

solidityTest :: Text -> Text -> Seq Text -> Seq Text
solidityTest name s seq =
  Seq.singleton ("function " <> name <> "() {") <>
    Seq.singleton ("  // " <> s) <>
    indent seq <>
    Seq.singleton "}"

pairs = Map.toAscList
forAllPairs x f = forM_ (pairs x) f

dump :: System -> Writer (Seq Claim) ()
dump sys = do
  write . WadIs "vox.fix()" $ Exactly (view (vat . fix) sys)
  write . WadIs "vox.par()" $ Exactly (view (vat . par) sys)
  write . RayIs "vox.way()" $ Exactly (view (vat . way) sys)
  write . RayIs "vox.how()" $ Exactly (view (vat . how) sys)
  forAllPairs (view (vat . jars) sys) $ \(Id i, j) -> do
    write
      . WadIs    ("vat.tag(jars[" <> txt i <> "])")
      $ Exactly  (view tag j)
    forAllPairs (view (gem . balanceOf) j) $ \(a, x) -> do
      let a' = case a of
                 InAccount (Address h) ->
                   "lads[" <> txt h <> "]"
                 InVault (Id h) ->
                   "jars[" <> txt h <> "]"
      write .
        WadIs ("jars[" <> txt i <> "].token().balanceOf(" <>
                a' <> ")") $ Exactly x
  forAllPairs (view (vat . ilks) sys) $ \(Id i, x) -> do
    write
      . RayIs ("vat.axe(id(" <> txt i <> "))")
      $ Exactly (view axe x)
    write
      . WadIs ("vat.hat(id(" <> txt i <> "))")
      $ Exactly (view hat x)
    write
      . SecIs ("vat.lax(id(" <> txt i <> "))")
      $ Exactly (view lag x)
    write
      . RayIs ("vat.mat(id(" <> txt i <> "))")
      $ Exactly (view mat x)
    write
      . RayIs ("vat.tax(id(" <> txt i <> "))")
      $ Exactly (view tax x)
    write
      . RayIs ("vat.chi(id(" <> txt i <> "))")
      $ Exactly (view chi x)
  forAllPairs (view (vat . urns) sys) $ \(Id i, x) -> do
    write
      . WadIs ("vat.art(lads[" <> txt i <> "].urn())")
      $ Exactly (view art x)
    write
      . WadIs ("vat.jam(lads[" <> txt i <> "].urn())")
      $ Exactly (view jam x)

soliditySystem = 
  dump  >>> execWriter  >>> fmap solidityClaim

chooseRay x y = fmap ray (lift (choose (x, y)))
chooseWad x y = fmap wad (lift (choose (x, y)))

testCaseX :: WriterT (Seq (Address, Act)) Gen ()
testCaseX = do
  x <- chooseRay 1 1.000000000000000001
  y <- chooseWad 1 1.01
  write (id_god, Frob x)
  write (id_god, Tell y)
  write (id_god, Warp (Sec 100))
  write (id_god, Prod)
  write (id_god, Mine (Id "DGX"))
  write (id_god, Sire (Address "Bob"))
  write (id_god, Hand (Address "Bob") (Wad 1) (Id "DGX"))
  write (id_god, Form (Id "DGX1") (Id "DGX"))
  write (Address "Bob", Open (Id "Bob") (Id "DGX1"))
  write (Address "Bob", Lock (Id "Bob") 0.5)

crossExamine :: Seq (Address, Act) -> (Bool, Seq Text)
crossExamine acts =
  case exec (initialSystem 1.0)
        (sequence_ (fmap (\(x, y) -> y `being` x) acts)) of
    Left x ->
      (False,  solidityTest "testFail_x" (txt x) (actsCode acts))
    Right x ->
      (True,   solidityTest "test_x" "" (actsCode acts <> soliditySystem x))

solidityFile :: Seq Text -> Text
solidityFile xs = intercalate "\n" $
  ["pragma solidity ^0.4.8;",
   "import \"./toy.sol\";",
   "contract FakerToy is MakerToy {",
   "  function setUp() {",
   "    this.par(1 ether);",
   "  }"]
  <> toList (indent xs) <>
  ["}", ""]
  
main :: IO ()
main = do
  x <- generate $ execWriterT testCaseX
  putStrLn . solidityFile . snd $ crossExamine x
