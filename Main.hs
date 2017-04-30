{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Main where

import System.IO (hPrint, stderr)

import Prelude (error)

import Maker.Prelude

import Maker hiding (Act (..))
import qualified Maker as Dai

import Data.ByteString.Lazy (getContents)
import Data.ByteString.Lazy.Char8 (putStrLn)

import Options.Generic

import System.Exit

instance ParseField Wad
instance ParseFields Wad
instance ParseRecord Wad where
  parseRecord = fmap getOnly parseRecord

instance ParseField Ray
instance ParseFields Ray
instance ParseRecord Ray where
  parseRecord = fmap getOnly parseRecord

instance ParseField Sec
instance ParseFields Sec
instance ParseRecord Sec where
  parseRecord = fmap getOnly parseRecord

data Command
  = Init
  
  | Open { lad :: String, urn :: String, ilk :: String }
  | Give { urn :: String, lad :: String }
  | Lock { urn :: String, wad :: Wad }
  | Free { urn :: String, wad :: Wad }
  | Draw { urn :: String, dai :: Wad }
  | Wipe { urn :: String, dai :: Wad }
  | Shut { urn :: String }
  
  | Prod
  | Drip { ilk :: String }
  
  | Mark { gem :: String, tag :: Wad, zzz :: Sec }
  | Tell { sdr :: Wad }

  | Bite { urn :: String }
  | Grab { urn :: String }
  | Plop { urn :: String, wad :: Wad }
  
  | Form { ilk :: String, gem :: String }
  | Frob { how :: Ray }
  | Warp { era :: Sec }
  | Mint { gem :: String, wad :: Wad, lad :: String }
  
  | Cuff { ilk :: String, mat :: Ray }
  | Chop { ilk :: String, axe :: Ray }
  | Cork { ilk :: String, hat :: Wad }
  | Calm { ilk :: String, lax :: Sec }
  deriving (Show, Generic, Eq)

instance ParseRecord Command

main = do
  x <- getRecord "Dai Credit System Simulator"
  if x == Init
    then putStrLn (encode (initialSystem 1.0))
    else
    do old <- fmap eitherDecode getContents
       case old of
         Left e -> error e
         Right Nothing -> error "error"
         Right (Just sys) -> do
           let run e m = 
                 case exec sys (being e (perform m)) of
                   Left x -> do
                     hPrint stderr x
                     putStrLn (encode (Nothing :: Maybe System))
                     exitFailure
                   Right sys' -> do
                     putStrLn (encode (Just sys'))
                     exitSuccess

               who x =
                 case view (vat . urns . at (Id x)) sys of
                   Nothing -> error "no such urn"
                   Just y -> view Dai.lad y
                   
           case x of
             Init -> error "not possible"
             
             Open x y z -> run (Account (Address x)) $
               Dai.Open (Id y) (Id z)
             Give x y -> run (who x) $
               Dai.Give (Id x) (Account (Address y))
             Lock x y -> run (who x) $
               Dai.Lock (Id x) y
             Free x y -> run (who x) $
               Dai.Free (Id x) y
             Draw x y -> run (who x) $
               Dai.Draw (Id x) y
             Wipe x y -> run (who x) $
               Dai.Wipe (Id x) y
             Shut x -> run (who x) $
               Dai.Shut (Id x)
               
             Prod -> run God $
               Dai.Prod
             Drip x -> run God $
               Dai.Drip x
               
             Form x y -> run God $
               Dai.Form (Id x) (Gem y)
             Mark x y z -> run God $
               Dai.Mark (Gem x) y z
             Tell x -> run God $
               Dai.Tell x
             Frob x -> run God $
               Dai.Frob x
             Warp x -> run God $
               Dai.Warp x
             Mint x y z -> run God $
               Dai.Mint (Gem x) y (Account (Address z))
             Cuff x y -> run God $
               Dai.Cuff (Id x) y
             Chop x y -> run God $
               Dai.Chop (Id x) y
             Cork x y -> run God $
               Dai.Cork (Id x) y
             Calm x y -> run God $
               Dai.Calm (Id x) y
