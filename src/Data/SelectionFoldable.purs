-- | A Foldable where at most one item is selected. The selected item is
-- | guaranteed to be in the Foldable structure. However, no guarantees are made
-- | of its uniqueness.

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
    ) where

import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.SelectionFoldableWithData (SelectionFoldableWithData, IsSelected)
import Data.SelectionFoldableWithData as SFWD
import Data.Tuple (snd)
import Prelude (class Eq, class Functor, Unit, unit)

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
selectWith = SFWD.selectWith unit

selectIndex :: forall i f a
    . FoldableWithIndex i f
    => Eq a
    => Eq i
    => i
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectIndex = SFWD.selectIndex unit

selectWithIndex :: forall i f a
    . FoldableWithIndex i f
    => (i -> a -> IsSelected)
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectWithIndex = SFWD.selectWithIndex unit

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
