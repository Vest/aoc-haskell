module Days.Day10 where

import Day10
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day10" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day10" ~: "parseLine(value 5 goes to bot 2)" ~: Just [IntValue 5, BotValue 2] ~=? Day10.parseLine "value 5 goes to bot 2",
      "day10" ~: "parseLine(bot 2 gives low to bot 1 and high to bot 0)" ~: Just [BotValue 2, BotValue 1, BotValue 0] ~=? Day10.parseLine "bot 2 gives low to bot 1 and high to bot 0",
      "day10" ~: "parseLine(bot 1 gives low to output 1 and high to bot 0)" ~: Just [BotValue 1, OutputValue 1, BotValue 0] ~=? Day10.parseLine "bot 1 gives low to output 1 and high to bot 0",
      "day10" ~: "parseLine(bot 0 gives low to output 2 and high to output 0)" ~: Just [BotValue 0, OutputValue 2, OutputValue 0] ~=? Day10.parseLine "bot 0 gives low to output 2 and high to output 0",
      "day10" ~: "parseLine(something Wrong)" ~: Nothing ~=? Day10.parseLine "something Wrong"
    ]
