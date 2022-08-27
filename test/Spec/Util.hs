module Spec.Util
( spec
)
where

import qualified CryptoDepth.OrderBook.Db.Util              as Util

import qualified Test.Hspec                                 as Hspec
import qualified Test.Hspec.Expectations                    as Expect
import qualified System.Random.Shuffle                      as Shuffle


spec :: Hspec.Spec
spec =
    Hspec.describe "sortByOccurrenceCount" $
        Hspec.it "works on unit test" $
            unitTest1

unitTest1 :: Hspec.Expectation
unitTest1 = do
    shuffledInput <- traverse Shuffle.shuffleM testCase1Input
    testCaseResult shuffledInput `Expect.shouldBe` testCase1Input
  where
    testCaseResult
        :: [[(Char, Int)]]
        -> [[(Char, Int)]]
    testCaseResult =
        Util.sortByOccurrenceCount fst snd

testCase1Input :: [[(Char, Int)]]
testCase1Input =
    [ [ ('a', 7), ('a', 6), ('a', 5), ('a', 4) ]
    , [ ('b', 7), ('b', 6), ('b', 5), ('b', 4) ]
    , [ ('c', 7), ('c', 6), ('c', 5), ('c', 4) ]
    , [ ('d', 7), ('d', 6), ('d', 5), ('d', 4) ]
    , [ ('e', 7), ('e', 6), ('e', 5), ('e', 4) ]
    , [ ('f', 7), ('f', 6), ('f', 5), ('f', 3) ]
    , [ ('g', 7), ('g', 6), ('g', 3), ('g', 2) ]
    , [ ('h', 7), ('h', 3), ('h', 2), ('h', 1) ]
    ]

{-

expected:
    [ [('a',7),('a',6),('a',5),('a',4)]
    , [('b',7),('b',6),('b',5),('b',4)]
    , [('c',7),('c',6),('c',5),('c',4)]
    , [('d',7),('d',6),('d',5),('d',4)]
    , [('e',7),('e',6),('e',5),('e',4)]
    , [('f',7),('f',6),('f',5),('f',3)]
    , [('g',7),('g',6),('g',3),('g',2)]
    , [('h',7),('h',3),('h',2),('h',1)]]

but got:
    [ [('a',4),('a',5),('a',6),('a',7)]
    , [('b',4),('b',5),('b',6),('b',7)]
    , [('c',4),('c',5),('c',6),('c',7)]
    , [('d',4),('d',5),('d',6),('d',7)]
    , [('e',4),('e',5),('e',6),('e',7)]
    , [('f',3),('f',5),('f',6),('f',7)]
    , [('g',2),('g',3),('g',6),('g',7)]
    , [('h',1),('h',2),('h',3),('h',7)]
    ]

-}