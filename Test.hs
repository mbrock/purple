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

import Control.Arrow ((>>>))
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad (sequence_)

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
  (_, Prod)    -> "this.prod();"
  (_, Frob x)  -> "this.frob(" <> txt (unray x) <> "); // " <> txt x
  (_, Tell x)  -> "this.tell(" <> txt (unwad x) <> "); // " <> txt x
  (_, Warp x)  -> "this.warp(" <> txt (unsec x) <> ");"
  (_, Form x y) -> "this.form(" <> txt x <> ", " <> txt y <> ");"
  (_, Mine (Id x))   -> "this.gem(" <> txt x <> ");"
  (_, Sire (Address x))   -> "this.lad(" <> txt x <> ");"
  (_, Hand (Address x) y (Id z))   -> "this.hand(" <> txt x <> ", " <> txt (unwad y) <> ", " <> txt z<> ");"

actsCode :: Seq (Address, Act) -> Seq Text
actsCode = fmap solidityAct

indent :: Seq Text -> Seq Text
indent = fmap ("  " <>)

solidityTest :: Text -> Seq Text -> Seq Text
solidityTest name seq =
  Seq.singleton ("function " <> name <> "() {") <>
    indent seq <>
    Seq.singleton "}"

dump :: System -> Writer (Seq Claim) ()
dump sys = do
  write . WadIs "vox.par()" $ Exactly (view (vat . par) sys)
  write . RayIs "vox.way()" $ Exactly (view (vat . way) sys)
  write . RayIs "vox.how()" $ Exactly (view (vat . how) sys)

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

crossExamine :: Seq (Address, Act) -> (Bool, Seq Text)
crossExamine acts =
  case exec (initialSystem 1.0)
        (sequence_ (fmap (perform . snd) acts)) of
    Left _ ->
      (False,  solidityTest "testFail_x" (actsCode acts))
    Right x ->
      (True,   solidityTest "test_x" (actsCode acts <> soliditySystem x))

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
