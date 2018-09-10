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
    , foldrWithIndexSelected
    , foldlSelected
    , foldlWithIndexSelected
    ) where

import Data.Compactable (compactDefault, separateDefault)
import Data.Either (Either(..))
import Data.Filterable (class Compactable, class Filterable, filter, filterMap, partition, partitionMap)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Prelude (class Eq, class Functor, class Show, bind, show, ($), (&&), (<>), (==), (>>=))

-- | A Foldable where at most one item is selected, and the selected item has
-- | some extra data associated with it. The selected item is guaranteed to be
-- | in the Foldable structure. However, no guarantees are made regarding
-- | uniqueness of the items (see the README for more info).
-- |
-- | The data constructor is kept private in order to maintain the desired
-- | invariants.
-- |
-- | - `f` is the type of the Foldable that will contain the items.
-- | - `d` is the type of the data associated with the optionally selected item.
-- | - `a` is the type of the items.
data SelectionFoldableWithData f d a
    = Private_ (f a) (Maybe (Tuple d a))

-- | A type alias used for better clarity in type signatures.
type IsSelected = Boolean

instance eqSelectionFoldableWithData :: (Eq (f a), Eq d, Eq a) => Eq (SelectionFoldableWithData f d a) where
    eq (Private_ xsL mSelL) (Private_ xsR mSelR) = (xsL == xsR) && (mSelL == mSelR)

instance showSelectionFoldableWithData :: (Show (f a), Show d, Show a) => Show (SelectionFoldableWithData f d a) where
    show (Private_ xs mSel) =
        "(SelectionFoldableWithData " <> show xs <> " " <> show mSel <> ")"

instance functorSelectionFoldableWithData :: Functor f => Functor (SelectionFoldableWithData f d) where
    map f (Private_ xs mSel) = Private_ (map f xs) (map (map f) mSel)

instance foldableSelectionFoldableWithData :: Foldable f => Foldable (SelectionFoldableWithData f d) where
    foldr f z (Private_ xs _) = foldr f z xs
    foldl f z (Private_ xs _) = foldl f z xs
    foldMap f (Private_ xs _) = foldMap f xs

instance foldableWithIndexSelectionFoldableWithData :: FoldableWithIndex i f => FoldableWithIndex i (SelectionFoldableWithData f d) where
    foldrWithIndex f z (Private_ xs _) = foldrWithIndex f z xs
    foldlWithIndex f z (Private_ xs _) = foldlWithIndex f z xs
    foldMapWithIndex f (Private_ xs _) = foldMapWithIndex f xs

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

-- | Constructs a `SelectionFoldableWithData` from a Foldable structure of
-- | items.
fromFoldable :: forall f d a. Foldable f => f a -> SelectionFoldableWithData f d a
fromFoldable xs = Private_ xs Nothing

-- | Extracts the Foldable structure of items from a
-- | `SelectionFoldableWithData`.
toFoldable :: forall f d a. Foldable f => SelectionFoldableWithData f d a -> f a
toFoldable (Private_ xs _) = xs

-- | Selects the first element `a` such that `a == x`. If such an element is
-- | found, it is selected and the provided `d` is used as the associated data.
-- | If not, nothing happens.
select :: forall f d a
    . Foldable f
    => Eq a
    => d
    -> a
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
select d x = selectWith (\x' -> if x' == x then Just d else Nothing)

-- | Selects the first element `a` such that `p a == true`. If such an element
-- | is found, it is selected and the provided function `f` is used to calculate
-- | the `d` to be used as the associated data. If not, nothing happens.
selectWith :: forall f d a
    . Foldable f
    => (a -> Maybe d)
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
selectWith f (Private_ xs curMSel) = (Private_ xs newMSel) where
    newMSel :: Maybe (Tuple d a)
    newMSel = case found of
        Just t -> Just $ Tuple t.d t.x
        Nothing -> curMSel

    found :: Maybe { d :: d, x :: a }
    found = foldl accFn Nothing xs

    accFn :: Maybe { d :: d, x :: a } -> a -> Maybe { d :: d, x :: a }
    accFn z x = case z of
        Just _ -> z
        Nothing -> map (\d -> { d, x }) (f x)

-- | Selects the element at index `i'` such that `i' == i`. If such an element
-- | is found, it is selected and the provided function `p` is used to calculate
-- | the `d` to be used as the associated data. If not, nothing happens.
selectIndex :: forall i f d a
    . FoldableWithIndex i f
    => Eq i
    => (a -> d)
    -> i
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
selectIndex f i =
    selectWithIndex (\i' x -> if i == i' then Just (f x) else Nothing)

-- | Selects the first element `a` such that `p i a == Just d` where `i` is the
-- | index of `a`. If such an element is found, it is selected and the provided
-- | `d` is used as the associated data. If not, nothing happens.
selectWithIndex :: forall i f d a
    . FoldableWithIndex i f
    => (i -> a -> Maybe d)
    -> SelectionFoldableWithData f d a
    -> SelectionFoldableWithData f d a
selectWithIndex f (Private_ xs curMSel) = (Private_ xs newMSel) where
    newMSel :: Maybe (Tuple d a)
    newMSel = case found of
        Just t -> Just (Tuple t.d t.x)
        Nothing -> curMSel

    found :: Maybe { i :: i, d :: d, x :: a }
    found = foldlWithIndex accFn Nothing xs

    accFn ::
        i
        -> Maybe { i :: i, d :: d, x :: a }
        -> a
        -> Maybe { i :: i, d :: d, x :: a }
    accFn i z x = case z of
        Just _ -> z
        Nothing -> map (\d -> { i, d, x }) (f i x)

-- | Clears the selection and its associated data.
deselect :: forall f d a. SelectionFoldableWithData f d a -> SelectionFoldableWithData f d a
deselect (Private_ xs _) = Private_ xs Nothing

-- | Returns the selected item and its associated data as a Tuple (if they
-- | exist).
selected :: forall f d a. SelectionFoldableWithData f d a -> Maybe (Tuple d a)
selected (Private_ _ mSel) = mSel

-- | Returns the selected item (if it exists).
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

-- | Performs a foldr, using the provided functions to transform the items and
-- | the data. The `sel` function is used for all items `a` that are equal to
-- | the selected item, and the `rest` function is used for the other items.
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

-- | Performs a foldrWithIndex, using the provided functions to transform the
-- | items and the data. The `sel` function is used for all items `a` that are
-- | equal to the selected item, and the `rest` function is used for the other
-- | items.
foldrWithIndexSelected :: forall i f d a b
    . FoldableWithIndex i f
    => Eq a
    => { sel :: i -> Tuple d a -> b -> b, rest :: i -> a -> b -> b }
    -> b
    -> SelectionFoldableWithData f d a
    -> b
foldrWithIndexSelected fns b (Private_ xs mSel) = foldrWithIndex accFn b xs where
    accFn :: i -> a -> b -> b
    accFn i x z = case mSel of
        Nothing ->
            fns.rest i x z

        Just sel ->
            if x == snd sel then
                fns.sel i sel z
            else
                fns.rest i x z

-- | Performs a foldl, using the provided functions to transform the items and
-- | the data. The `sel` function is used for all items `a` that are equal to
-- | the selected item, and the `rest` function is used for the other items.
foldlSelected :: forall f d a b
    . Foldable f
    => Eq a
    => { sel :: b -> Tuple d a -> b, rest :: b -> a -> b }
    -> b
    -> SelectionFoldableWithData f d a
    -> b
foldlSelected fns b (Private_ xs mSel) = foldl accFn b xs where
    accFn :: b -> a -> b
    accFn z x = case mSel of
        Nothing ->
            fns.rest z x

        Just sel ->
            if x == snd sel then
                fns.sel z sel
            else
                fns.rest z x

-- | Performs a foldlWithIndex, using the provided functions to transform the
-- | items and the data. The `sel` function is used for all items `a` that are
-- | equal to the selected item, and the `rest` function is used for the other
-- | items.
foldlWithIndexSelected :: forall i f d a b
    . FoldableWithIndex i f
    => Eq a
    => { sel :: i -> b -> Tuple d a -> b, rest :: i -> b -> a -> b }
    -> b
    -> SelectionFoldableWithData f d a
    -> b
foldlWithIndexSelected fns b (Private_ xs mSel) = foldlWithIndex accFn b xs where
    accFn :: i -> b -> a -> b
    accFn i z x = case mSel of
        Nothing ->
            fns.rest i z x

        Just sel ->
            if x == snd sel then
                fns.sel i z sel
            else
                fns.rest i z x
