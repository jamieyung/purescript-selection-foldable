-- | A Foldable where at most one item is selected, and the selected item has
-- | some extra data associated with it. The selected item is guaranteed to be in
-- | the Foldable structure. However, no guarantees are made of its uniqueness.

module Data.SelectionFoldableWithData
    ( SelectionFoldableWithData
    , IsSelected
    , fromFoldable
    , toFoldable
    , select
    , selectWith
    , selectIndex
    , selectWithIndex
    , deselect
    , selected
    , selected_
    , mapSelected
    , foldrSelected
    ) where

import Data.Compactable (compactDefault, separateDefault)
import Data.Either (Either(..))
import Data.Filterable (class Compactable, class Filterable, filter, filterMap, partition, partitionMap)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Prelude (class Eq, class Functor, class Show, bind, show, ($), (&&), (<>), (==), (>>=))

data SelectionFoldableWithData f d a
    = Private_ (f a) (Maybe (Tuple d a))

type IsSelected = Boolean

instance eqSelectionFoldableWithData :: (Eq (f a), Eq d, Eq a) => Eq (SelectionFoldableWithData f d a) where
    eq (Private_ xsL mSelL) (Private_ xsR mSelR) = (xsL == xsR) && (mSelL == mSelR)

instance showSelectionFoldableWithData :: (Show (f a), Show d, Show a) => Show (SelectionFoldableWithData f d a) where
    show (Private_ xs mSel) =
        "(SelectionFoldableWithData " <> show xs <> " " <> show mSel <> ")"

instance functorSelectionFoldableWithData :: Functor f => Functor (SelectionFoldableWithData f d) where
    map f (Private_ xs mSel) = Private_ (map f xs) (map (map f) mSel)

instance compactableSelectionFoldableWithData :: (Functor f, Compactable f) => Compactable (SelectionFoldableWithData f d) where
    compact = compactDefault
    separate = separateDefault

instance filterableSelectionFoldable :: Filterable f => Filterable (SelectionFoldableWithData f d) where
    partitionMap f (Private_ xs mSel) =
        { left: Private_ xsL mSelL
        , right: Private_ xsR mSelR
        }
        where
        { left: xsL, right: _ } = partitionMap f xs
        { left: _, right: xsR } = partitionMap f xs
        mSelR = mSel >>= \(Tuple d a) -> case f a of
            Left _ -> Nothing
            Right x -> Just $ Tuple d x
        mSelL = mSel >>= \(Tuple d a) -> case f a of
            Left x -> Just $ Tuple d x
            Right _ -> Nothing

    partition f (Private_ xs mSel) =
        { no: Private_ xsL mSelL
        , yes: Private_ xsR mSelR
        }
        where
        { no: xsL, yes: _ } = partition f xs
        { no: _, yes: xsR } = partition f xs
        mSelL = mSel >>= \(Tuple _ a) -> if f a then Nothing else mSel
        mSelR = mSel >>= \(Tuple _ a) -> if f a then mSel else Nothing

    filterMap f (Private_ xs mSel) =
        Private_ (filterMap f xs) mSel' where
        mSel' = do
            (Tuple d a) <- mSel
            b <- f a
            Just $ Tuple d b

    filter f (Private_ xs mSel) =
        Private_ (filter f xs) (mSel >>= \(Tuple _ a) -> if f a then mSel else Nothing)

fromFoldable :: forall f d a. Foldable f => f a -> SelectionFoldableWithData f d a
fromFoldable xs = Private_ xs Nothing

toFoldable :: forall f d a. Foldable f => SelectionFoldableWithData f d a -> f a
toFoldable (Private_ xs _) = xs

-- | Selects the first element `a` such that `a == x`.
select :: forall f d a
    . Foldable f
    => Eq a
    => d
    -> a
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
select d x = selectWith d (_ == x)

-- | Selects the first element `a` such that `p a == true`.
selectWith :: forall f d a
    . Foldable f
    => d
    -> (a -> IsSelected)
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
selectWith d p (Private_ xs curMSel) = (Private_ xs newMSel) where
    newMSel :: Maybe (Tuple d a)
    newMSel = case found of
        Just x -> Just $ Tuple d x
        Nothing -> curMSel

    found :: Maybe a
    found = foldl accFn Nothing xs

    accFn :: Maybe a -> a -> Maybe a
    accFn z x = case z of
        Just _ -> z
        Nothing -> if p x then Just x else Nothing

selectIndex :: forall i f d a
    . FoldableWithIndex i f
    => Eq a
    => Eq i
    => d
    -> i
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
selectIndex d i = selectWithIndex d (\i' _ -> i == i')

selectWithIndex :: forall i f d a
    . FoldableWithIndex i f
    => d
    -> (i -> a -> IsSelected)
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
selectWithIndex d p (Private_ xs curMSel) = (Private_ xs newMSel) where
    newMSel :: Maybe (Tuple d a)
    newMSel = case found of
        Just (Tuple i x) -> Just (Tuple d x)
        Nothing -> curMSel

    found :: Maybe (Tuple i a)
    found = foldlWithIndex accFn Nothing xs

    accFn :: i -> Maybe (Tuple i a) -> a -> Maybe (Tuple i a)
    accFn i z x = case z of
        Just _ -> z
        Nothing -> if p i x then Just (Tuple i x) else Nothing

deselect :: forall f d a. SelectionFoldableWithData f d a -> SelectionFoldableWithData f d a
deselect (Private_ xs _) = Private_ xs Nothing

selected :: forall f d a. SelectionFoldableWithData f d a -> Maybe (Tuple d a)
selected (Private_ _ mSel) = mSel

selected_ :: forall f d a. SelectionFoldableWithData f d a -> Maybe a
selected_ (Private_ _ mSel) = map snd mSel

-- | If there exist multiple elements `a` such that `p a == true`, the function
-- | will be invoked with `true` as the first argument. No guarantee of
-- | uniqueness is made; that is left up to the user.
mapSelected :: forall f d a e b
    . Foldable f
    => Functor f
    => Eq a
    => { sel :: Tuple d a -> Tuple e b, rest :: a -> b }
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f e b
mapSelected fns (Private_ xs mSel) = Private_ (map f xs) mSel' where
    mSel' = map fns.sel mSel

    f x =
        if Just x == map snd mSel then
            fromMaybe (fns.rest x) (map snd mSel')
        else
            fns.rest x

foldrSelected :: forall f d a b
    . Foldable f
    => Eq a
    => { sel :: Tuple d a -> b -> b, rest :: a -> b -> b }
    -> b
    -> SelectionFoldableWithData f d a
    -> b
foldrSelected fns b (Private_ xs mSel) = foldr accFn b xs where
    accFn :: a -> b -> b
    accFn x z = case mSel of
        Nothing ->
            fns.rest x z

        Just sel ->
            if x == snd sel then
                fns.sel sel z
            else
                fns.rest x z
