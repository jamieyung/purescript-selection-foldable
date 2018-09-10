module Test.SelectionFoldable where

import Data.Array (foldr, (:))
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(..))
import Data.SelectionFoldable as SF
import Prelude (Unit, discard, map, show, (#), (+), (<>), (==), (>), (>=))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = describe "SelectionFoldable" do
    describe "Eq instance" do
        it "Structures equal, selected items equal -> equal" do
            shouldEqual
                (SF.fromFoldable [1])
                (SF.fromFoldable [1])

        it "Structures equal, selected items not equal -> not equal (1)" do
            shouldNotEqual
                (SF.fromFoldable [1, 2] # SF.select 1)
                (SF.fromFoldable [1, 2] # SF.select 2)

        it "Structures equal, selected items not equal -> not equal (2)" do
            shouldNotEqual
                (SF.fromFoldable [1, 2] # SF.select 1)
                (SF.fromFoldable [1, 2]) -- Selected Nothing

        it "Structures not equal, selected items equal -> not equal" do
            shouldNotEqual
                (SF.fromFoldable [1] # SF.select 1)
                (SF.fromFoldable [1, 2] # SF.select 1)

        it "Structures not equal, selected items not equal -> not equal (1)" do
            shouldNotEqual
                (SF.fromFoldable [1] # SF.select 1)
                (SF.fromFoldable [2] # SF.select 2)

        it "Structures not equal, selected items not equal -> not equal (2)" do
            shouldNotEqual
                (SF.fromFoldable [1] # SF.select 1)
                (SF.fromFoldable [2]) -- Selected Nothing

    describe "Functor instance" do
        it "map works only on the structure if nothing selected" do
            shouldEqual
                (SF.fromFoldable [1,2,3]
                    # map (_ + 1)
                    # show
                )
                ("(SelectionFoldableWithData [2,3,4] Nothing)")

        it "map works on the structure and the selected item" do
            shouldEqual
                (SF.fromFoldable [1,2,3]
                    # SF.select 1
                    # map (_ + 1)
                    # show
                )
                ("(SelectionFoldableWithData [2,3,4] (Just (Tuple unit 2)))")

    describe "Foldable instance" do
        it "foldr" do
            shouldEqual
                ((SF.fromFoldable [1,2,3])
                    # foldr (\n z -> show n <> z) ""
                )
                ("123")

    describe "FoldableWithIndex instance" do
        it "foldrWithIndex" do
            shouldEqual
                ((SF.fromFoldable ["a","b","c"])
                    # foldrWithIndex (\i c z -> c <> show i <> z) ""
                )
                ("a0b1c2")

    describe "Filterable instance" do
        it "partitionMap" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3] # partitionMap \n ->
                    if n >= 2 then Left (show n) else Right (show n))
                ({ left: SF.fromFoldable ["2", "3"]
                , right: SF.fromFoldable ["1"]
                })

        it "partition" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3] # partition (_ >= 2))
                { yes: SF.fromFoldable [2, 3]
                , no: SF.fromFoldable [1]
                }

        it "filterMap works only on the structure if nothing selected" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3]
                    # filterMap \n -> if n > 1 then Just (show n) else Nothing)
                (SF.fromFoldable ["2", "3"])

        it "filterMap works on the selected item" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3]
                    # SF.select 2
                    # filterMap (\n -> if n > 1 then Just (show n) else Nothing)
                    # show)
                ("(SelectionFoldableWithData [\"2\",\"3\"] (Just (Tuple unit \"2\")))")

        it "filter works only on the structure if nothing selected" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3]
                    # filter (_ > 1))
                (SF.fromFoldable [2, 3])

        it "filter retains the selected item if p(selected) == true" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3]
                    # SF.select 2
                    # filter (_ > 1)
                    # show)
                ("(SelectionFoldableWithData [2,3] (Just (Tuple unit 2)))")

        it "filter removes the selected item if p(selected) == false" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3]
                    # SF.select 1
                    # filter (_ > 1)
                    # show)
                ("(SelectionFoldableWithData [2,3] Nothing)")

    describe "fromFoldable" do
        it "fromFoldable" do
            shouldEqual
                (SF.fromFoldable [1] # show)
                ("(SelectionFoldableWithData [1] Nothing)")

    describe "toFoldable" do
        it "toFoldable" do
            shouldEqual
                (SF.fromFoldable [1] # SF.toFoldable)
                ([1])

    describe "select" do
        it "match" do
            shouldEqual
                (SF.fromFoldable [1] # SF.select 1 # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match" do
            shouldEqual
                (SF.fromFoldable [1] # SF.select 2 # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "select overrides any previous selection" do
            shouldEqual
                (SF.fromFoldable [1, 2] # SF.select 1 # SF.select 2 # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 2)))")

        it "attempting to select an element not found in the structure does nothing" do
            shouldEqual
                (SF.fromFoldable [1, 2] # SF.select 1 # SF.select 5 # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 1)))")

    describe "selectWith" do
        it "match" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectWith (_ == 1) # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectWith (_ == 2) # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "selects the first match" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3] # SF.selectWith (_ >= 2) # show)
                ("(SelectionFoldableWithData [1,2,3] (Just (Tuple unit 2)))")

    describe "selectIndex" do
        it "match" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectIndex 0 # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectIndex 1 # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "select overrides any previous selection" do
            shouldEqual
                (SF.fromFoldable [1, 2] # SF.selectIndex 0 # SF.selectIndex 1 # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 2)))")

        it "attempting to select an element not found in the structure does nothing" do
            shouldEqual
                (SF.fromFoldable [1, 2] # SF.selectIndex 0 # SF.selectIndex 5 # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 1)))")

    describe "selectWithIndex" do
        it "match index" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectWithIndex (\i _ -> i == 0) # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "match element" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectWithIndex (\_ x -> x == 1) # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match index" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectWithIndex (\i _ -> i == 2) # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "no match element" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selectWithIndex (\_ x -> x == 2) # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "selects the first match" do
            shouldEqual
                (SF.fromFoldable [1, 2, 3] # SF.selectWithIndex (\i _ -> i >= 1) # show)
                ("(SelectionFoldableWithData [1,2,3] (Just (Tuple unit 2)))")

    describe "deselect" do
        it "does nothing if nothing selected" do
            shouldEqual
                (SF.fromFoldable [1] # SF.deselect)
                (SF.fromFoldable [1])

        it "deselects the selected item" do
            shouldEqual
                (SF.fromFoldable [1] # SF.select 1 # SF.deselect)
                (SF.fromFoldable [1])

    describe "selected" do
        it "returns Nothing if nothing selected" do
            shouldEqual
                (SF.fromFoldable [1] # SF.selected)
                (Nothing)

        it "returns the selected item" do
            shouldEqual
                (SF.fromFoldable [1] # SF.select 1 # SF.selected)
                (Just 1)

    describe "mapSelected" do
        it "works only on the structure if nothing selected" do
            shouldEqual
                (SF.fromFoldable [1,2,3]
                    # SF.mapSelected (\isSelected n ->
                        if isSelected then n + 1 else n + 1)
                    # show
                )
                ("(SelectionFoldableWithData [2,3,4] Nothing)")

        it "applies the selected fn to the selected item" do
            shouldEqual
                (SF.fromFoldable [1,2,3]
                    # SF.select 1
                    # SF.mapSelected (\isSelected n ->
                        if isSelected then n + 10 else n + 1)
                    # show
                )
                ("(SelectionFoldableWithData [11,3,4] (Just (Tuple unit 11)))")

        it "applies the selected fn to all items equal to the selected item" do
            shouldEqual
                (SF.fromFoldable [1,1,1]
                    # SF.select 1
                    # SF.mapSelected (\isSelected n ->
                        if isSelected then n + 10 else n + 1)
                    # show
                )
                ("(SelectionFoldableWithData [11,11,11] (Just (Tuple unit 11)))")

    describe "foldrSelected" do
        it "foldrSelected" do
            shouldEqual
                (SF.fromFoldable [1,2,3]
                    # SF.select 1
                    # SF.foldrSelected (\isSelected x z ->
                        if isSelected then
                            (show x <> "!") : z
                        else
                            (show x) : z
                    ) []
                )
                (["1!","2","3"])

    describe "foldrWithIndexSelected" do
        it "foldrWithIndexSelected" do
            shouldEqual
                (SF.fromFoldable ["a", "b", "c"]
                    # SF.select "a"
                    # SF.foldrWithIndexSelected (\isSelected i x z ->
                        if isSelected then
                            (x <> show i <> "!") : z
                        else
                            (x <> show i) : z
                    ) []
                )
                (["a0!", "b1", "c2"])

    describe "foldlSelected" do
        it "foldlSelected" do
            shouldEqual
                (SF.fromFoldable [1,2,3]
                    # SF.select 1
                    # SF.foldlSelected (\isSelected z x ->
                        if isSelected then
                            (show x <> "!") : z
                        else
                            (show x) : z
                    ) []
                )
                (["3","2","1!"])

    describe "foldlWithIndexSelected" do
        it "foldlWithIndexSelected" do
            shouldEqual
                (SF.fromFoldable ["a", "b", "c"]
                    # SF.select "a"
                    # SF.foldlWithIndexSelected (\isSelected i z x ->
                        if isSelected then
                            (x <> show i <> "!") : z
                        else
                            (x <> show i) : z
                    ) []
                )
                (["c2", "b1", "a0!"])
