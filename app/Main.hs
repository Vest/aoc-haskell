module Main where

import Advent (getAnswers)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (getArgs, lookupEnv)
import System.Exit

main :: IO ()
main = do
  token <- getToken
  case token of
    Just token -> do
      putStrLn ("token is: " ++ token)
      print (Advent.getAnswers "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4")
    Nothing -> do
      putStrLn "Cannot find a token, use either -h for more details or ADVENT_SESSION as env variable"
      exitFailure

getToken :: IO (Maybe String)
getToken = do
  token <- lookupEnv "ADVENT_SESSION"
  case token of
    Nothing -> do
      args <- getArgs
      fromArgs <- parse args
      return fromArgs
    Just token -> do
      return $ Just token

{- taken from https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling#Getting_in_arguments -}
parse [] = do
  return Nothing
parse ["-h"] = do
  putStrLn "Usage: aoc-haskell [-h] [1234]"
  exitSuccess
parse [s] = do
  return $ Just s
parse _ = do
  putStrLn "Cannot read aoc token: aoc-haskell [-h] [1234]"
  exitFailure
