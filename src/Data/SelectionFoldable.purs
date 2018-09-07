module Data.SelectionFoldable
    ( SelectionFoldable
    , IsSelected
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

import Data.Compactable (compactDefault, separateDefault)
import Data.Either (Either(..), hush)
import Data.Filterable (class Compactable, class Filterable, filter, filterMap, partition, partitionMap)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Functor, class Show, show, ($), (&&), (<>), (==), (>>=))

data SelectionFoldable f a
    = Private_ (f a) (Maybe a)

type IsSelected = Boolean

instance eqSelectionFoldable :: (Eq a, Eq (f a)) => Eq (SelectionFoldable f a) where
    eq (Private_ lxs lMSel) (Private_ rxs rMSel) = (lxs == rxs) && (lMSel == rMSel)

instance showSelectionFoldable :: (Show a, Show (f a)) => Show (SelectionFoldable f a) where
    show (Private_ xs mSel) =
        "(SelectionFoldable " <> show xs <> " " <> show mSel <> ")"

instance functorSelectionFoldable :: Functor f => Functor (SelectionFoldable f) where
    map f (Private_ xs mSel) = Private_ (Functor.map f xs) (Functor.map f mSel)

instance compactableSelectionFoldable :: (Functor f, Compactable f) => Compactable (SelectionFoldable f) where
    compact = compactDefault
    separate = separateDefault

instance filterableSelectionFoldable :: Filterable f => Filterable (SelectionFoldable f) where
    partitionMap f (Private_ xs mSel) =
        { left: Private_ lxs lMSel
        , right: Private_ rxs rMSel
        }
        where
        { left: lxs, right: _ } = partitionMap f xs
        { left: _, right: rxs } = partitionMap f xs
        rMSel = mSel >>= \sel -> hush $ f sel
        lMSel = mSel >>= \sel -> case f sel of
            Left x -> Just x
            Right _ -> Nothing

    partition f (Private_ xs mSel) =
        { no: Private_ lxs lMSel
        , yes: Private_ rxs rMSel
        }
        where
        { no: lxs, yes: _ } = partition f xs
        { no: _, yes: rxs } = partition f xs
        lMSel = mSel >>= \sel -> if f sel then Nothing else mSel
        rMSel = mSel >>= \sel -> if f sel then mSel else Nothing

    filterMap f (Private_ xs mSel) =
        Private_ (filterMap f xs) (mSel >>= f)

    filter f (Private_ xs mSel) =
        Private_ (filter f xs) (mSel >>= \sel -> if f sel then mSel else Nothing)

fromFoldable :: forall f a. Foldable f => Eq a => f a -> SelectionFoldable f a
fromFoldable xs = Private_ xs Nothing

toFoldable :: forall f a. Foldable f => Eq a => SelectionFoldable f a -> f a
toFoldable (Private_ xs _) = xs

{-
    Selects the first element `a` such that `a == x`.
-}
select :: forall f a
    . Foldable f
    => Eq a
    => a
    -> SelectionFoldable f a
    -> SelectionFoldable f a
select x = selectWith (_ == x)

{-
    Selects the first element `a` such that `p a == true`.
-}
selectWith :: forall f a
    . Foldable f
    => Eq a
    => (a -> IsSelected)
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectWith p (Private_ xs curMSel) = (Private_ xs newMSel) where
    newMSel :: Maybe a
    newMSel = case found of
        Just x -> Just x
        Nothing -> curMSel

    found :: Maybe a
    found = foldl accFn Nothing xs

    accFn :: Maybe a -> a -> Maybe a
    accFn z x = case z of
        Just _ -> z
        Nothing -> if p x then Just x else Nothing

selectIndex :: forall i f a
    . FoldableWithIndex i f
    => Eq a
    => Eq i
    => i
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectIndex i = selectWithIndex (\i' _ -> i == i')

selectWithIndex :: forall i f a
    . FoldableWithIndex i f
    => Eq a
    => (i -> a -> IsSelected)
    -> SelectionFoldable f a
    -> SelectionFoldable f a
selectWithIndex p (Private_ xs curMSel) = (Private_ xs newMSel) where
    newMSel :: Maybe a
    newMSel = case found of
        Just x -> Just x
        Nothing -> curMSel

    found :: Maybe a
    found = foldlWithIndex accFn Nothing xs

    accFn :: i -> Maybe a -> a -> Maybe a
    accFn i z x = case z of
        Just _ -> z
        Nothing -> if p i x then Just x else Nothing

deselect :: forall f a. SelectionFoldable f a -> SelectionFoldable f a
deselect (Private_ xs _) = Private_ xs Nothing

selected :: forall f a. SelectionFoldable f a -> Maybe a
selected (Private_ _ mSel) = mSel

{-
    If there exist multiple elements `a` such that `p a == true`, the function
    will be invoked with `true` as the first argument. No guarantee of
    uniqueness is made; that is left up to the user.
-}
mapSelected :: forall f a b
    . Foldable f
    => Functor f
    => Eq a
    => (IsSelected -> a -> b)
    -> SelectionFoldable f a
    -> SelectionFoldable f b
mapSelected f (Private_ xs mSel) = Private_ xs' mSel' where
    xs' = Functor.map (\x -> if Just x == mSel then f true x else f false x) xs
    mSel' = Functor.map (f true) mSel

foldrSelected :: forall f a b
    . Foldable f
    => Eq a
    => (IsSelected -> a -> b -> b)
    -> b
    -> SelectionFoldable f a
    -> b
foldrSelected f z (Private_ xs mSel) = foldr accFn z xs where
    accFn :: a -> b -> b
    accFn x z = case mSel of
        Nothing ->
            f false x z

        Just sel ->
            f (x == sel) x z
