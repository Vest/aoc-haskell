module Days.Day10 where

import qualified Data.Map as M
import Day10
import Test.HUnit

tests' :: Test
tests' =
  let firstSample =
        unlines
          [ "value 5 goes to bot 2",
            "bot 2 gives low to bot 1 and high to bot 0",
            "value 3 goes to bot 1",
            "bot 1 gives low to output 1 and high to bot 0",
            "bot 0 gives low to output 2 and high to output 0",
            "value 2 goes to bot 2"
          ]
   in test
        [ "day10" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
          "day10" ~: "parseLine(value 5 goes to bot 2)"
            ~: Just
              [ IntValue 5,
                BotValue 2
              ]
            ~=? Day10.parseLine "value 5 goes to bot 2",
          "day10" ~: "parseLine(bot 2 gives low to bot 1 and high to bot 0)"
            ~: Just
              [ BotValue 2,
                BotValue 1,
                BotValue 0
              ]
            ~=? Day10.parseLine "bot 2 gives low to bot 1 and high to bot 0",
          "day10" ~: "parseLine(bot 1 gives low to output 1 and high to bot 0)"
            ~: Just
              [ BotValue 1,
                OutputValue 1,
                BotValue 0
              ]
            ~=? Day10.parseLine "bot 1 gives low to output 1 and high to bot 0",
          "day10" ~: "parseLine(bot 0 gives low to output 2 and high to output 0)"
            ~: Just
              [ BotValue 0,
                OutputValue 2,
                OutputValue 0
              ]
            ~=? Day10.parseLine "bot 0 gives low to output 2 and high to output 0",
          "day10" ~: "parseInput(sample)"
            ~: ( M.fromList
                   [ (0, Bot {low = OutputLocation 2, high = OutputLocation 0}),
                     (1, Bot {low = OutputLocation 1, high = BotLocation 0}),
                     (2, Bot {low = BotLocation 1, high = BotLocation 0})
                   ],
                 M.fromList
                   [ (5, BotLocation 2),
                     (3, BotLocation 1),
                     (2, BotLocation 2)
                   ]
               )
            ~=? Day10.parseInput firstSample,
          "day10" ~: "findTwoNumbersByBot(sample)" ~: (2, 5)
            ~=? ( Day10.findTwoNumbersByBot . M.fromList $
                    [ (5, BotLocation 2),
                      (3, BotLocation 1),
                      (2, BotLocation 2)
                    ]
                ),
          "day10" ~: "makeStep(sampleData)"
            ~: M.fromList
              [ (5, OutputLocation 0),
                (3, OutputLocation 2),
                (2, OutputLocation 1)
              ]
            ~=? case Day10.parseInput firstSample of (rules, values) -> foldl (\vals _ -> Day10.makeStep rules vals) values [1 .. 3]
        ]
