{-# Language LambdaCase #-}

import Maker
import IPPrint.Colored
import System.Console.Readline

say = putStrLn

repl sys = do
  say ""
  readline "> " >>= \case
    Nothing -> return ()
    Just line -> do
      let a = read line
          sys' = exec (Id "God") sys (perform a)
      report sys'
      repl sys'

main :: IO ()
main = do
  say "Maker Control Room"
  say "You see a strange financial system here."
  repl initialSystem
  
