module Test.SelectionFoldableWithData where

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(..))
import Data.SelectionFoldableWithData (SelectionFoldableWithData)
import Data.SelectionFoldableWithData as SFWD
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, discard, map, show, unit, (#), (+), (<>), (==), (>), (>=))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

type SFArrUnitInt = SelectionFoldableWithData Array Unit Int

spec :: Spec Unit
spec = describe "SelectionFoldableWithData" do
    describe "Eq instance" do
        it "Structures equal, selected items equal -> equal" do
            shouldEqual
                (SFWD.fromFoldable [1] :: SFArrUnitInt)
                (SFWD.fromFoldable [1] :: SFArrUnitInt)

        it "Structures equal, selected items not equal -> not equal (1)" do
            shouldNotEqual
                (SFWD.fromFoldable [1, 2] # SFWD.select unit 1)
                (SFWD.fromFoldable [1, 2] # SFWD.select unit 2)

        it "Structures equal, selected items not equal -> not equal (2)" do
            shouldNotEqual
                (SFWD.fromFoldable [1, 2] # SFWD.select unit 1)
                (SFWD.fromFoldable [1, 2]) -- Selected Nothing

        it "Structures not equal, selected items equal -> not equal" do
            shouldNotEqual
                (SFWD.fromFoldable [1] # SFWD.select unit 1)
                (SFWD.fromFoldable [1, 2] # SFWD.select unit 1)

        it "Structures not equal, selected items not equal -> not equal (1)" do
            shouldNotEqual
                (SFWD.fromFoldable [1] # SFWD.select unit 1)
                (SFWD.fromFoldable [2] # SFWD.select unit 2)

        it "Structures not equal, selected items not equal -> not equal (2)" do
            shouldNotEqual
                (SFWD.fromFoldable [1] # SFWD.select unit 1)
                (SFWD.fromFoldable [2]) -- Selected Nothing

    describe "Functor instance" do
        it "map works only on the structure if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1,2,3] :: SFArrUnitInt)
                    # map (_ + 1)
                    # show
                )
                ("(SelectionFoldableWithData [2,3,4] Nothing)")

        it "map works on the structure and the selected item" do
            shouldEqual
                (SFWD.fromFoldable [1,2,3]
                    # SFWD.select unit 1
                    # map (_ + 1)
                    # show
                )
                ("(SelectionFoldableWithData [2,3,4] (Just (Tuple unit 2)))")

    describe "Foldable instance" do
        it "foldr" do
            shouldEqual
                ((SFWD.fromFoldable [1,2,3] :: SFArrUnitInt)
                    # foldr (\n z -> show n <> z) ""
                )
                ("123")

    describe "FoldableWithIndex instance" do
        it "foldrWithIndex" do
            shouldEqual
                ((SFWD.fromFoldable ["a","b","c"])
                    # foldrWithIndex (\i c z -> c <> show i <> z) ""
                )
                ("a0b1c2")

    describe "Filterable instance" do
        it "partitionMap" do
            shouldEqual
                ((SFWD.fromFoldable [1, 2, 3] :: SFArrUnitInt)
                    # partitionMap \n ->
                        if n >= 2 then Left (show n) else Right (show n))
                ({ left: SFWD.fromFoldable ["2", "3"]
                , right: SFWD.fromFoldable ["1"]
                })

        it "partition" do
            shouldEqual
                ((SFWD.fromFoldable [1, 2, 3] :: SFArrUnitInt)
                # partition (_ >= 2))
                { yes: SFWD.fromFoldable [2, 3]
                , no: SFWD.fromFoldable [1]
                }

        it "filterMap works only on the structure if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1, 2, 3] :: SFArrUnitInt)
                    # filterMap \n -> if n > 1 then Just (show n) else Nothing)
                (SFWD.fromFoldable ["2", "3"])

        it "filterMap works on the selected item" do
            shouldEqual
                (SFWD.fromFoldable [1, 2, 3]
                    # SFWD.select unit 2
                    # filterMap (\n -> if n > 1 then Just (show n) else Nothing)
                    # show)
                ("(SelectionFoldableWithData [\"2\",\"3\"] (Just (Tuple unit \"2\")))")

        it "filter works only on the structure if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1, 2, 3] :: SFArrUnitInt)
                    # filter (_ > 1))
                (SFWD.fromFoldable [2, 3])

        it "filter retains the selected item if p(selected) == true" do
            shouldEqual
                (SFWD.fromFoldable [1, 2, 3]
                    # SFWD.select unit 2
                    # filter (_ > 1)
                    # show)
                ("(SelectionFoldableWithData [2,3] (Just (Tuple unit 2)))")

        it "filter removes the selected item if p(selected) == false" do
            shouldEqual
                (SFWD.fromFoldable [1, 2, 3]
                    # SFWD.select unit 1
                    # filter (_ > 1)
                    # show)
                ("(SelectionFoldableWithData [2,3] Nothing)")

    describe "fromFoldable" do
        it "fromFoldable" do
            shouldEqual
                ((SFWD.fromFoldable [1] :: SFArrUnitInt) # show)
                ("(SelectionFoldableWithData [1] Nothing)")

    describe "toFoldable" do
        it "toFoldable" do
            shouldEqual
                (SFWD.fromFoldable [1] # SFWD.toFoldable)
                ([1])

    describe "select" do
        it "match" do
            shouldEqual
                (SFWD.fromFoldable [1] # SFWD.select unit 1 # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match" do
            shouldEqual
                (SFWD.fromFoldable [1] # SFWD.select unit 2 # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "select overrides any previous selection" do
            shouldEqual
                (SFWD.fromFoldable [1, 2] # SFWD.select unit 1 # SFWD.select unit 2 # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 2)))")

        it "attempting to select an element not found in the structure does nothing" do
            shouldEqual
                ((SFWD.fromFoldable [1, 2] :: SFArrUnitInt)
                    # SFWD.select unit 1 # SFWD.select unit 5 # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 1)))")

    describe "selectWith" do
        it "match" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectWith
                        (\x -> if x == 1 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectWith
                        (\x -> if x == 2 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "selects the first match" do
            shouldEqual
                (SFWD.fromFoldable [1, 2, 3]
                    # SFWD.selectWith
                        (\x -> if x >= 2 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1,2,3] (Just (Tuple unit 2)))")

    describe "selectIndex" do
        it "match" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectIndex (const unit) 0 # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectIndex (const unit) 1 # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "select overrides any previous selection" do
            shouldEqual
                (SFWD.fromFoldable [1, 2]
                    # SFWD.selectIndex (const unit) 0
                    # SFWD.selectIndex (const unit) 1
                    # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 2)))")

        it "attempting to select an element not found in the structure does nothing" do
            shouldEqual
                (SFWD.fromFoldable [1, 2]
                    # SFWD.selectIndex (const unit) 0
                    # SFWD.selectIndex (const unit) 5
                    # show)
                ("(SelectionFoldableWithData [1,2] (Just (Tuple unit 1)))")

    describe "selectWithIndex" do
        it "match index" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectWithIndex
                        (\i _ -> if i == 0 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "match element" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectWithIndex
                        (\_ x -> if x == 1 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1] (Just (Tuple unit 1)))")

        it "no match index" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectWithIndex
                        (\i _ -> if i == 2 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "no match element" do
            shouldEqual
                (SFWD.fromFoldable [1]
                    # SFWD.selectWithIndex
                        (\_ x -> if x == 2 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1] Nothing)")

        it "selects the first match" do
            shouldEqual
                (SFWD.fromFoldable [1, 2, 3]
                    # SFWD.selectWithIndex
                        (\i _ -> if i >= 1 then Just unit else Nothing)
                    # show)
                ("(SelectionFoldableWithData [1,2,3] (Just (Tuple unit 2)))")

    describe "deselect" do
        it "does nothing if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1] :: SFArrUnitInt) # SFWD.deselect)
                ((SFWD.fromFoldable [1] :: SFArrUnitInt))

        it "deselects the selected item" do
            shouldEqual
                (SFWD.fromFoldable [1] # SFWD.select unit 1 # SFWD.deselect)
                (SFWD.fromFoldable [1])

    describe "selected" do
        it "returns Nothing if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1] :: SFArrUnitInt) # SFWD.selected)
                (Nothing)

        it "returns the selected item" do
            shouldEqual
                ((SFWD.fromFoldable [1] :: SFArrUnitInt) # SFWD.select unit 1 # SFWD.selected)
                (Just (Tuple unit 1))

    describe "selected_" do
        it "returns Nothing if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1] :: SFArrUnitInt) # SFWD.selected_)
                (Nothing)

        it "returns the selected item" do
            shouldEqual
                ((SFWD.fromFoldable [1] :: SFArrUnitInt) # SFWD.select unit 1 # SFWD.selected_)
                (Just 1)

    describe "mapSelected" do
        it "works only on the structure if nothing selected" do
            shouldEqual
                ((SFWD.fromFoldable [1,2,3] :: SFArrUnitInt)
                    # SFWD.mapSelected
                        { sel: \(Tuple d n) -> Tuple d (n + 10)
                        , rest: \n -> n + 1
                        }
                    # show
                )
                ("(SelectionFoldableWithData [2,3,4] Nothing)")

        it "applies the selected fn to the selected item" do
            shouldEqual
                ((SFWD.fromFoldable [1,2,3] :: SFArrUnitInt)
                    # SFWD.select unit 1
                    # SFWD.mapSelected
                        { sel: \(Tuple d n) -> Tuple d (n + 10)
                        , rest: \n -> n + 1
                        }
                    # show
                )
                ("(SelectionFoldableWithData [11,3,4] (Just (Tuple unit 11)))")

        it "applies the selected fn to all items equal to the selected item" do
            shouldEqual
                (SFWD.fromFoldable [1,1,1]
                    # SFWD.select unit 1
                    # SFWD.mapSelected
                        { sel: \(Tuple d n) -> Tuple d (n + 10)
                        , rest: \n -> n + 1
                        }
                    # show
                )
                ("(SelectionFoldableWithData [11,11,11] (Just (Tuple unit 11)))")

    describe "foldrSelected" do
        it "foldrSelected" do
            shouldEqual
                (SFWD.fromFoldable [1,2,3]
                    # SFWD.select "!" 1
                    # SFWD.foldrSelected
                        { sel: \(Tuple s x) z -> (show x <> s) : z
                        , rest: \x z -> (show x) : z
                        } []
                )
                (["1!","2","3"])

    describe "foldrWithIndexSelected" do
        it "foldrWithIndexSelected" do
            shouldEqual
                (SFWD.fromFoldable ["a", "b", "c"]
                    # SFWD.select "!" "a"
                    # SFWD.foldrWithIndexSelected
                        { sel: \i (Tuple s x) z -> (x <> show i <> s) : z
                        , rest: \i x z -> (x <> show i) : z
                        } []
                )
                (["a0!", "b1", "c2"])

    describe "foldlSelected" do
        it "foldlSelected" do
            shouldEqual
                (SFWD.fromFoldable [1,2,3]
                    # SFWD.select "!" 1
                    # SFWD.foldlSelected
                        { sel: \z (Tuple s x) -> (show x <> s) : z
                        , rest: \z x -> (show x) : z
                        } []
                )
                (["3","2","1!"])

    describe "foldlWithIndexSelected" do
        it "foldlWithIndexSelected" do
            shouldEqual
                (SFWD.fromFoldable ["a", "b", "c"]
                    # SFWD.select "!" "a"
                    # SFWD.foldlWithIndexSelected
                        { sel: \i z (Tuple s x) -> (x <> show i <> s) : z
                        , rest: \i z x -> (x <> show i) : z
                        } []
                )
                (["c2", "b1", "a0!"])
