module Main where

import Festival

main :: IO ()
main = do
  initialize (InitConf True 6000000)
  sayText "This is a test."
