module Main where

import Advent (getAnswers)
import System.Environment (getArgs, lookupEnv)
import System.Exit

main :: IO ()
main = do
  token <- getToken
  case token of
    Just token' -> do
      putStrLn ("token is: " ++ token')
      answers <- Advent.getAnswers token'
      mapM_ (\(day, answer) -> putStrLn ("Day " ++ show day ++ ": " ++ answer)) $
        zip [1 ..] answers
      exitSuccess
    Nothing -> do
      putStrLn "Cannot find a token, use either -h for more details or ADVENT_SESSION as env variable"
      exitFailure

getToken :: IO (Maybe String)
getToken = do
  token <- lookupEnv "ADVENT_SESSION"
  case token of
    Nothing -> getArgs >>= parse
    Just token' -> return $ Just token'

{- taken from https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling#Getting_in_arguments -}
parse :: [String] -> IO (Maybe String)
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
