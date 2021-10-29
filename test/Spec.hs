import Days.Day1 as D1
import Days.Day2 as D2
import Days.Day3 as D3
import Days.Day4 as D4
import Days.Day5 as D5
import Test.HUnit

foo :: Int -> (Int, Int)
foo x = (1, x)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1, 3) (foo 3))

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = do
  _ <- runTestTT tests
  runTestTT D1.tests'
  runTestTT D2.tests'
  runTestTT D3.tests'
  runTestTT D4.tests'
  runTestTT D5.tests'
