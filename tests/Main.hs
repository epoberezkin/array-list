{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Array
import Data.Array.IsList
import Test.Hspec

main :: IO ()
main = hspec do
  describe "array-list" do
    isListTest
    arrayNestedListTest

isListTest :: SpecWith ()
isListTest = describe "IsList" do
  describe "IsList (Array Int e)" do
    it "[1,2,3]" $
      ([1, 2, 3] :: Array Int Int)
        `shouldBe` array (0, 2) [(0, 1), (1, 2), (2, 3)]
    it "[\"one\",\"two\",\"three\"]" $
      (["one", "two", "three"] :: Array Int String)
        `shouldBe` array (0, 2) [(0, "one"), (1, "two"), (2, "three")]
    it "toList $ fromList [1,2,3]" $
      toList ([1, 2, 3] :: Array Int Int)
        `shouldBe` [1, 2, 3]
    it "toList $ fromList [\"one\",\"two\",\"three\"]" $
      toList (["one", "two", "three"] :: Array Int String)
        `shouldBe` ["one", "two", "three"]
  describe "IsList (Array (Int,Int) e)" do
    it "[[1, 2], [3, 4], [5, 6]]" $
      ( [ [1, 2],
          [3, 4],
          [5, 6]
        ] ::
          Array (Int, Int) Int
      )
        `shouldBe` array
          ((0, 0), (2, 1))
          [ ((0, 0), 1),
            ((0, 1), 2),
            ((1, 0), 3),
            ((1, 1), 4),
            ((2, 0), 5),
            ((2, 1), 6)
          ]
    it "toList . fromList == id" $
      toList
        ( [ [1, 2],
            [3, 4],
            [5, 6]
          ] ::
            Array (Int, Int) Int
        )
        `shouldBe` [[1, 2], [3, 4], [5, 6]]
  describe "IsList (Array (Int,Int,Int) e)" do
    it "3D-array" $
      ( [ [[1, 2, 3], [4, 5, 6]],
          [[7, 8, 9], [10, 11, 12]],
          [[13, 14, 15], [16, 17, 18]]
        ] ::
          Array (Int, Int, Int) Int
      )
        `shouldBe` array
          ((0, 0, 0), (2, 1, 2))
          [ ((0, 0, 0), 1),
            ((0, 0, 1), 2),
            ((0, 0, 2), 3),
            ((0, 1, 0), 4),
            ((0, 1, 1), 5),
            ((0, 1, 2), 6),
            ((1, 0, 0), 7),
            ((1, 0, 1), 8),
            ((1, 0, 2), 9),
            ((1, 1, 0), 10),
            ((1, 1, 1), 11),
            ((1, 1, 2), 12),
            ((2, 0, 0), 13),
            ((2, 0, 1), 14),
            ((2, 0, 2), 15),
            ((2, 1, 0), 16),
            ((2, 1, 1), 17),
            ((2, 1, 2), 18)
          ]
    it "toList . fromList == id" $
      toList
        ( [ [[1, 2, 3], [4, 5, 6]],
            [[7, 8, 9], [10, 11, 12]],
            [[13, 14, 15], [16, 17, 18]]
          ] ::
            Array (Int, Int, Int) Int
        )
        `shouldBe` [ [[1, 2, 3], [4, 5, 6]],
                     [[7, 8, 9], [10, 11, 12]],
                     [[13, 14, 15], [16, 17, 18]]
                   ]
  describe "IsList (Array (Int,Int,Int,Int) e)" do
    it "4D-array" $
      ( [ [ [[0, 1], [2, 3]],
            [[4, 5], [6, 7]]
          ],
          [ [[8, 9], [10, 11]],
            [[12, 13], [14, 15]]
          ]
        ] ::
          Array (Int, Int, Int, Int) Int
      )
        `shouldBe` array
          ((0, 0, 0, 0), (1, 1, 1, 1))
          [ ((0, 0, 0, 0), 0),
            ((0, 0, 0, 1), 1),
            ((0, 0, 1, 0), 2),
            ((0, 0, 1, 1), 3),
            ((0, 1, 0, 0), 4),
            ((0, 1, 0, 1), 5),
            ((0, 1, 1, 0), 6),
            ((0, 1, 1, 1), 7),
            ((1, 0, 0, 0), 8),
            ((1, 0, 0, 1), 9),
            ((1, 0, 1, 0), 10),
            ((1, 0, 1, 1), 11),
            ((1, 1, 0, 0), 12),
            ((1, 1, 0, 1), 13),
            ((1, 1, 1, 0), 14),
            ((1, 1, 1, 1), 15)
          ]
    it "toList . fromList == id" $
      toList
        ( [ [ [[0, 1], [2, 3]],
              [[4, 5], [6, 7]]
            ],
            [ [[8, 9], [10, 11]],
              [[12, 13], [14, 15]]
            ]
          ] ::
            Array (Int, Int, Int, Int) Int
        )
        `shouldBe` [ [ [[0, 1], [2, 3]],
                       [[4, 5], [6, 7]]
                     ],
                     [ [[8, 9], [10, 11]],
                       [[12, 13], [14, 15]]
                     ]
                   ]
  describe "IsList (Array (Int,Int,Int,Int,Int) e)" do
    it "5D-array" $
      ( [ [ [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
            [[[8, 9], [10, 11]], [[12, 13], [14, 15]]]
          ],
          [ [[[16, 17], [18, 19]], [[20, 21], [22, 23]]],
            [[[24, 25], [26, 27]], [[28, 29], [30, 31]]]
          ]
        ] ::
          Array (Int, Int, Int, Int, Int) Int
      )
        `shouldBe` array
          ((0, 0, 0, 0, 0), (1, 1, 1, 1, 1))
          [ ((0, 0, 0, 0, 0), 0),
            ((0, 0, 0, 0, 1), 1),
            ((0, 0, 0, 1, 0), 2),
            ((0, 0, 0, 1, 1), 3),
            ((0, 0, 1, 0, 0), 4),
            ((0, 0, 1, 0, 1), 5),
            ((0, 0, 1, 1, 0), 6),
            ((0, 0, 1, 1, 1), 7),
            ((0, 1, 0, 0, 0), 8),
            ((0, 1, 0, 0, 1), 9),
            ((0, 1, 0, 1, 0), 10),
            ((0, 1, 0, 1, 1), 11),
            ((0, 1, 1, 0, 0), 12),
            ((0, 1, 1, 0, 1), 13),
            ((0, 1, 1, 1, 0), 14),
            ((0, 1, 1, 1, 1), 15),
            ((1, 0, 0, 0, 0), 16),
            ((1, 0, 0, 0, 1), 17),
            ((1, 0, 0, 1, 0), 18),
            ((1, 0, 0, 1, 1), 19),
            ((1, 0, 1, 0, 0), 20),
            ((1, 0, 1, 0, 1), 21),
            ((1, 0, 1, 1, 0), 22),
            ((1, 0, 1, 1, 1), 23),
            ((1, 1, 0, 0, 0), 24),
            ((1, 1, 0, 0, 1), 25),
            ((1, 1, 0, 1, 0), 26),
            ((1, 1, 0, 1, 1), 27),
            ((1, 1, 1, 0, 0), 28),
            ((1, 1, 1, 0, 1), 29),
            ((1, 1, 1, 1, 0), 30),
            ((1, 1, 1, 1, 1), 31)
          ]
    it "toList . fromList == id" $
      toList
        ( [ [ [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
              [[[8, 9], [10, 11]], [[12, 13], [14, 15]]]
            ],
            [ [[[16, 17], [18, 19]], [[20, 21], [22, 23]]],
              [[[24, 25], [26, 27]], [[28, 29], [30, 31]]]
            ]
          ] ::
            Array (Int, Int, Int, Int, Int) Int
        )
        `shouldBe` [ [ [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
                       [[[8, 9], [10, 11]], [[12, 13], [14, 15]]]
                     ],
                     [ [[[16, 17], [18, 19]], [[20, 21], [22, 23]]],
                       [[[24, 25], [26, 27]], [[28, 29], [30, 31]]]
                     ]
                   ]

arrayNestedListTest :: SpecWith ()
arrayNestedListTest = describe "ArrayNestedList" do
  describe "ArrayNestedList (Bool,Bool) e" do
    it "[[1, 2], [3, 4]]" $
      ( arrayNestedList
          ((False, False), (True, True))
          [[1, 2], [3, 4]] ::
          Array (Bool, Bool) Int
      )
        `shouldBe` array
          ((False, False), (True, True))
          [ ((False, False), 1),
            ((False, True), 2),
            ((True, False), 3),
            ((True, True), 4)
          ]
    it "toNestedList . arrayNestedList == id" $
      toNestedList
        ( arrayNestedList
            ((False, False), (True, True))
            [[1, 2], [3, 4]] ::
            Array (Bool, Bool) Int
        )
        `shouldBe` [[1, 2], [3, 4]]
  describe "ArrayNestedList (Bool,Bool,Bool) e" do
    it "3D-array" $
      ( arrayNestedList
          ((False, False, False), (True, True, True))
          [ [[1, 2], [3, 4]],
            [[5, 6], [7, 8]]
          ] ::
          Array (Bool, Bool, Bool) Int
      )
        `shouldBe` array
          ((False, False, False), (True, True, True))
          [ ((False, False, False), 1),
            ((False, False, True), 2),
            ((False, True, False), 3),
            ((False, True, True), 4),
            ((True, False, False), 5),
            ((True, False, True), 6),
            ((True, True, False), 7),
            ((True, True, True), 8)
          ]
    it "toNestedList . arrayNestedList == id" $
      toNestedList
        ( arrayNestedList
            ((False, False, False), (True, True, True))
            [ [[1, 2], [3, 4]],
              [[5, 6], [7, 8]]
            ] ::
            Array (Bool, Bool, Bool) Int
        )
        `shouldBe` [ [[1, 2], [3, 4]],
                     [[5, 6], [7, 8]]
                   ]
  describe "ArrayNestedList (Bool,Bool,Bool,Bool) e" do
    it "4D-array" $
      ( arrayNestedList
          ((False, False, False, False), (True, True, True, True))
          [ [ [[0, 1], [2, 3]],
              [[4, 5], [6, 7]]
            ],
            [ [[8, 9], [10, 11]],
              [[12, 13], [14, 15]]
            ]
          ] ::
          Array (Bool, Bool, Bool, Bool) Int
      )
        `shouldBe` array
          ((False, False, False, False), (True, True, True, True))
          [ ((False, False, False, False), 0),
            ((False, False, False, True), 1),
            ((False, False, True, False), 2),
            ((False, False, True, True), 3),
            ((False, True, False, False), 4),
            ((False, True, False, True), 5),
            ((False, True, True, False), 6),
            ((False, True, True, True), 7),
            ((True, False, False, False), 8),
            ((True, False, False, True), 9),
            ((True, False, True, False), 10),
            ((True, False, True, True), 11),
            ((True, True, False, False), 12),
            ((True, True, False, True), 13),
            ((True, True, True, False), 14),
            ((True, True, True, True), 15)
          ]
    it "toNestedList . arrayNestedList == id" $
      toNestedList
        ( arrayNestedList
            ((False, False, False, False), (True, True, True, True))
            [ [ [[0, 1], [2, 3]],
                [[4, 5], [6, 7]]
              ],
              [ [[8, 9], [10, 11]],
                [[12, 13], [14, 15]]
              ]
            ] ::
            Array (Bool, Bool, Bool, Bool) Int
        )
        `shouldBe` [ [ [[0, 1], [2, 3]],
                       [[4, 5], [6, 7]]
                     ],
                     [ [[8, 9], [10, 11]],
                       [[12, 13], [14, 15]]
                     ]
                   ]
  describe "ArrayNestedList (Bool,Bool,Bool,Bool,Bool) e" do
    it "5D-array" $
      ( arrayNestedList
          ((False, False, False, False, False), (True, True, True, True, True))
          [ [ [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
              [[[8, 9], [10, 11]], [[12, 13], [14, 15]]]
            ],
            [ [[[16, 17], [18, 19]], [[20, 21], [22, 23]]],
              [[[24, 25], [26, 27]], [[28, 29], [30, 31]]]
            ]
          ] ::
          Array (Bool, Bool, Bool, Bool, Bool) Int
      )
        `shouldBe` array
          ((False, False, False, False, False), (True, True, True, True, True))
          [ ((False, False, False, False, False), 0),
            ((False, False, False, False, True), 1),
            ((False, False, False, True, False), 2),
            ((False, False, False, True, True), 3),
            ((False, False, True, False, False), 4),
            ((False, False, True, False, True), 5),
            ((False, False, True, True, False), 6),
            ((False, False, True, True, True), 7),
            ((False, True, False, False, False), 8),
            ((False, True, False, False, True), 9),
            ((False, True, False, True, False), 10),
            ((False, True, False, True, True), 11),
            ((False, True, True, False, False), 12),
            ((False, True, True, False, True), 13),
            ((False, True, True, True, False), 14),
            ((False, True, True, True, True), 15),
            ((True, False, False, False, False), 16),
            ((True, False, False, False, True), 17),
            ((True, False, False, True, False), 18),
            ((True, False, False, True, True), 19),
            ((True, False, True, False, False), 20),
            ((True, False, True, False, True), 21),
            ((True, False, True, True, False), 22),
            ((True, False, True, True, True), 23),
            ((True, True, False, False, False), 24),
            ((True, True, False, False, True), 25),
            ((True, True, False, True, False), 26),
            ((True, True, False, True, True), 27),
            ((True, True, True, False, False), 28),
            ((True, True, True, False, True), 29),
            ((True, True, True, True, False), 30),
            ((True, True, True, True, True), 31)
          ]
    it "toNestedList . arrayNestedList == id" $
      toNestedList
        ( arrayNestedList
            ((False, False, False, False, False), (True, True, True, True, True))
            [ [ [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
                [[[8, 9], [10, 11]], [[12, 13], [14, 15]]]
              ],
              [ [[[16, 17], [18, 19]], [[20, 21], [22, 23]]],
                [[[24, 25], [26, 27]], [[28, 29], [30, 31]]]
              ]
            ] ::
            Array (Bool, Bool, Bool, Bool, Bool) Int
        )
        `shouldBe` [ [ [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
                       [[[8, 9], [10, 11]], [[12, 13], [14, 15]]]
                     ],
                     [ [[[16, 17], [18, 19]], [[20, 21], [22, 23]]],
                       [[[24, 25], [26, 27]], [[28, 29], [30, 31]]]
                     ]
                   ]
