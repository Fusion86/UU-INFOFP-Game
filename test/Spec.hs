import Collision
import Model
import Test.HUnit

-- Pro gamer tip: use https://www.math10.com/en/geometry/geogebra/geogebra.html to create your test cases.

lineIntersectTests :: Test
lineIntersectTests =
  TestList
    [ TestCase (assertEqual "linesIntersect" (Just (1, 1)) (linesIntersect ((0, 0), (2, 2)) ((0, 2), (2, 0)))),
      TestCase (assertEqual "linesIntersect" (Just (0.4, 1.2)) (linesIntersect ((0, 2), (1, 0)) ((0, 0), (1, 3)))),
      TestCase (assertEqual "linesIntersect" Nothing (linesIntersect ((0, 2), (1, 1)) ((0, 0), (3, 2)))),
      TestCase (assertEqual "linesIntersect" Nothing (linesIntersect ((0, 2), (2, 1)) ((1, 2), (3, 3)))),
      TestCase (assertEqual "linesIntersect" (Just (1, 2)) (linesIntersect ((0, 2), (6, 2)) ((1, 2), (3, 3))))
    ]

lineIntersectsObjectTests :: Test
lineIntersectsObjectTests =
  TestList
    [ TestCase (assertEqual "lineIntersectsObject" Nothing (lineIntersectsObject ((5, 5), (5, 6)) (Box2D (2, 2) (2, 2)))),
      TestCase (assertEqual "lineIntersectsObject" (Just (2.0, 1.25)) (lineIntersectsObject ((1, 1), (5, 2)) (Box2D (2, 0) (2, 2)))),
      TestCase (assertEqual "lineIntersectsObject" (Just (2.5, 2)) (lineIntersectsObject ((3, 3), (2, 1)) (Box2D (2, 0) (2, 2))))
    ]

main :: IO ()
main = do
  runTestTT lineIntersectTests
  runTestTT lineIntersectsObjectTests
  return ()
