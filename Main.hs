{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Main where

import Prelude (error)

import Maker.Prelude
import qualified Maker as Dai

import Data.ByteString.Lazy (getContents)
import Data.ByteString.Lazy.Char8 (putStrLn)

import Options.Generic

data Command
  = Init
  | Form String String
  deriving (Show, Generic)

instance ParseRecord Command

main = do
  x <- getRecord "Dai Credit System Simulator"
  
  let
    run m = do old <- fmap eitherDecode getContents
               case old of
                 Left e -> error e
                 Right sys -> do
                   case Dai.exec sys m of
                     Left _ -> error "execution error"
                     Right sys' -> putStrLn (encode sys')
                     
  case x of
    Init -> putStrLn (encode (Dai.initialSystem 1.0))
    Form x y -> run $ Dai.form (Dai.Id x) (Dai.Gem y)
