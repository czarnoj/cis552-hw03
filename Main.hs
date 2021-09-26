{-
HW 3 - Typeclasses
==================

This homework assignment is composed of three problems.

Make all of your edits in the files [`Kata.hs`](Kata.hs),
[`SortedList.hs`](SortedList.hs) and [`MergeSort.hs`](MergeSort.hs)
found in your github repository.

Problem - Return of the Data Munging Kata
-----------------------------------------
See [`Kata.lhs`](Kata.html)

Problem - Sorted lists and type abstraction
-------------------------------------------
See [`SortedList.lhs`](SortedList.html)

Problem - MergeSort for foldable data structures
------------------------------------------------
See [`MergeSort.lhs`](MergeSort.html)
-}

-- obligatory main
module Main where

import Kata (testParseEventChar)
import MergeSort
  ( testCrispy,
    testDivide,
    testDivideList,
    testSortedFromList,
    testSortedFromList',
    testSumOfProducts,
  )
import SortedList
  ( testCount,
    testListMonoid,
    testMinimum,
    testNumDistinct,
    testSortedList,
  )
import Test.HUnit (Test (TestList), runTestTT)

{-
>
-}

main :: IO ()
main = do
  _ <- runTestTT $ TestList [testParseEventChar]
  _ <- runTestTT $ TestList [testListMonoid, testSortedList, testMinimum, testNumDistinct, testCount]
  _ <-
    runTestTT $
      TestList
        [ testSortedFromList,
          testSortedFromList',
          testSumOfProducts,
          testCrispy,
          testDivide,
          testDivideList
        ]
  return ()
