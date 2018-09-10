-- | See the `SelectionFoldableWithData` module docs for function comments, as
-- | all of the functions in this module are convenience aliases for the ones
-- | in that module (for the case where there is no data).

module Data.SelectionFoldable
    ( SelectionFoldable
    , fromFoldable
    , toFoldable
    , select
    , selectWith
    , selectIndex
    , selectWithIndex
    , deselect
    , selected
    , mapSelected
    , foldrSelected
    , foldrWithIndexSelected
    , foldlSelected
    , foldlWithIndexSelected
    ) where

import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.SelectionFoldableWithData (IsSelected, SelectionFoldableWithData)
import Data.SelectionFoldableWithData as SFWD
import Data.Tuple (snd)
import Prelude (class Eq, class Functor, Unit, const, unit)

-- | A Foldable where at most one item is selected. This is an alias for a
-- | `SelectionFoldableWithData` that has no associated data.
-- |
-- | - `f` is the type of the Foldable that will contain the items.
-- | - `a` is the type of the items.
type SelectionFoldable f a = SelectionFoldableWithData f Unit a

fromFoldable :: forall f a. Foldable f => f a -> SelectionFoldable f a
fromFoldable = SFWD.fromFoldable

toFoldable :: forall f a. Foldable f => SelectionFoldable f a -> f a
toFoldable = SFWD.toFoldable

select :: forall f a
    . Foldable f
    => Eq a
    => a
    -> SelectionFoldable f a
    -> SelectionFoldable f a
select = SFWD.select unit

selectWith :: forall f a
    . Foldable f
    => (a -> IsSelected)
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectWith p = SFWD.selectWith \x -> if p x then Just unit else Nothing

selectIndex :: forall i f a
    . FoldableWithIndex i f
    => Eq i
    => i
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectIndex = SFWD.selectIndex (const unit)

selectWithIndex :: forall i f a
    . FoldableWithIndex i f
    => (i -> a -> IsSelected)
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectWithIndex p =
    SFWD.selectWithIndex \i x -> if p i x then Just unit else Nothing

deselect :: forall f a. SelectionFoldable f a -> SelectionFoldable f a
deselect = SFWD.deselect

selected :: forall f a. SelectionFoldable f a -> Maybe a
selected = SFWD.selected_

mapSelected :: forall f a b
    . Foldable f
    => Functor f
    => Eq a
    => (IsSelected -> a -> b)
    -> SelectionFoldable f a
    -> SelectionFoldable f b
mapSelected f = SFWD.mapSelected { sel: map (f true), rest: f false }

foldrSelected :: forall f a b
    . Foldable f
    => Eq a
    => (IsSelected -> a -> b -> b)
    -> b
    -> SelectionFoldable f a
    -> b
foldrSelected f b =
    SFWD.foldrSelected { sel: \t z -> f true (snd t) z, rest: f false } b

foldrWithIndexSelected :: forall i f a b
    . FoldableWithIndex i f
    => Eq a
    => (IsSelected -> i -> a -> b -> b)
    -> b
    -> SelectionFoldable f a
    -> b
foldrWithIndexSelected f b =
    SFWD.foldrWithIndexSelected
        { sel: \i t z -> f true i (snd t) z, rest: f false } b

foldlSelected :: forall f a b
    . Foldable f
    => Eq a
    => (IsSelected -> b -> a -> b)
    -> b
    -> SelectionFoldable f a
    -> b
foldlSelected f b =
    SFWD.foldlSelected { sel: \z t -> f true z (snd t), rest: f false } b

foldlWithIndexSelected :: forall i f a b
    . FoldableWithIndex i f
    => Eq a
    => (IsSelected -> i -> b -> a -> b)
    -> b
    -> SelectionFoldable f a
    -> b
foldlWithIndexSelected f b =
    SFWD.foldlWithIndexSelected
        { sel: \i z t -> f true i z (snd t), rest: f false } b
