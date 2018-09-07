# SelectionFoldable

The `SelectionFoldable` type represents a Foldable structure (eg. Array, List)
of items where zero or one of the items is selected.

Also provided is the
`SelectionFoldableWithData` type, which also keeps some user-provided data along
with the selected item (the `SelectionFoldable` type is actually just a type
alias for a `SelectionFoldableWithData` where there is no data).

Inspired by the excellent Elm
[list-selection](https://github.com/NoRedInk/list-selection) package written by
[NoRedInk](https://github.com/NoRedInk).

## Usage examples

**Note 1**: All of the code examples in this README assume the following
imports (common imports like Prelude are implicit):

```purescript
import Data.SelectionFoldable as SF
import Data.SelectionFoldableWithData as SFWD
```

**Note 2**: These examples use the `#` operator, which is left-to-right function
application:

```purescript
1 # (\n -> n + 2) -- Add 2
-- 3

[1,2,3] # map (\n -> n + 1) -- Add one to each number
-- [2,3,4]

[1,2,3]
    # map (\n -> n + 1) -- 1. Add one to each number, then
    # map (\n -> n < 4) -- 2. Map the numbers to bools
-- [true,true,false]
```

#### Creating:

Any Foldable structure can be plugged in. Items and associated data can be of
any type:

```purescript
-- Using an Array:
x1 :: SF.SelectionFoldable Array Int
x1 = SF.fromFoldable [1,2,3]

-- Using a List:
x2 :: SF.SelectionFoldable List String
x2 = SF.fromFoldable (Cons "a" Nil)

-- This seems a bit silly, but it's possible!
x3 :: SF.SelectionFoldable Maybe Boolean
x3 = SF.fromFoldable (Just true)

-- Here `Bool` is the type of user-provided data associated with selected items.
-- The compiler will often require you to provide this type annotation.
x4 :: SFWD.SelectionFoldableWithData Array Bool Int
x4 = SFWD.fromFoldable [1,2,3]

-- Any types will work!
data Foo = A | B
x5 :: SFWD.SelectionFoldableWithData Array Foo Foo
x5 = SFWD.fromFoldable [A,B]
```

#### Selecting by equality:

Using `select` or `selectIndex` will select items by equality to the provided
item (this obviously requires an `Eq` instance for the item type):

```purescript
x1 :: SF.SelectionFoldable Array Int
x1 = SF.fromFoldable [1,3,9] # SF.select 3 -- Selects the second element
-- SF.selected x1 = Just 3 :: Maybe Int

x2 :: SF.SelectionFoldable Array Int
x2 = SF.fromFoldable [1,3,9] # SF.select 2 -- Selects nothing
-- SF.selected x2 = Nothing :: Maybe Int

x3 :: SF.SelectionFoldable Array String
x3 = SF.fromFoldable ["a","b","c"] # SF.selectIndex 0 -- Selects the 'a'
-- SF.selected x3 = Just "a" :: Maybe String

x4 :: SF.SelectionFoldable Array Int
x4 = SF.fromFoldable [1,3,9] # SF.select 10 -- Selects nothing (out of bounds)
-- SF.selected x4 = Nothing :: Maybe Int

x4 :: SF.SelectionFoldableWithData Array String Int
x4 = SF.fromFoldable [1, 0, 2]
    # SF.select "woo!" 2 -- Selects the 2, and stores the string "woo!" with it
-- SF.selected x4 = Just (Tuple "woo!" 2) :: Maybe (Tuple String Int)
```

#### Selecting with a function:

Using `selectWith` or `selectWithIndex` will select items using the provided
function:

```purescript
x1 :: SF.SelectionFoldable Array Int
x1 = SF.fromFoldable [1,3,9]
    # SF.selectWith (_ > 1) -- Selects the second element
-- SF.selected x1 = Just 3 :: Maybe Int

x2 :: SF.SelectionFoldable Array Int
x2 = SF.fromFoldable [1,3,9]
    # SF.selectWith (_ < 1) -- Selects nothing
-- SF.selected x2 = Nothing :: Maybe Int

x3 :: SF.SelectionFoldable Array Int
x3 = SF.fromFoldable [1, 0, 2]
    # SF.selectWithIndex (\i s -> i == s) -- Selects the 2
-- SF.selected x3 = Just 2 :: Maybe Int
```

This does not require an `Eq` instance for the item type to work:

```purescript
data Foo = A | B -- no Eq instance for Foo

x1 :: SF.SelectionFoldable Array String
x1 = SF.fromFoldable [A, B]
    # SF.selectWith (\x -> case x of
        A -> false
        B -> true
    ) -- selects the B

x2 :: Maybe Int
x2 = SF.selected x1
    # map (\x -> case x of
        A -> 1
        B -> 2
    ) -- Just 2 :: Maybe Int
```

#### Mapping:

The selected item (if it exists) gets mapped as well as the items in the
structure:

```purescript
x1 :: SF.SelectionFoldable Array Int
x1 = SF.fromFoldable [1,3,9]
    # SF.selectIndex 0 -- Selects the '1'
    # map (\n -> n + 1)
-- SF.toFoldable x1 = [2,4,10] :: Array Int
-- SF.selected x1 = Just 2 :: Maybe Int

x2 :: SF.SelectionFoldable Array Bool
x2 = SF.fromFoldable [1,3,9]
    # SF.select 10 -- Selects nothing (out of bounds)
    # map (\n -> n + 1)
-- SF.toFoldable x1 = [2,4,10] :: Array Int
-- SF.selected x1 = Nothing :: Maybe Int
```

#### Folding:

```purescript
SF.fromFoldable [1,2,3]
    # SF.select 1
    # SF.foldrSelected (\isSelected x z ->
        if isSelected then
            (show x <> "!") : z
        else
            (show x) : z
    ) []
-- ["1!","2","3"] :: Array String

SFWD.fromFoldable [1,2,3]
    # SFWD.select "!" 1
    # SFWD.foldrSelected
        { sel: \(Tuple s x) z -> (show x <> s) : z
        , rest: \x z -> (show x) : z
        } []
-- ["1!","2","3"] :: Array String
```

See `test/Test/Main.purs` for further examples.

## Invariants

#### It's impossible to select an item that isn't found in the Foldable structure.

This guarantee comes out of the fact that the data
constructor is private, and the exposed functions maintain the invariant.

## Regarding uniqueness of items

#### No guarantees are made with regards to the uniqueness of items in the structure.

Using the `select` function (or `selectWith`, etc) selects the first matching
item as expected:

```purescript
xs = SF.fromFoldable [1,2,3] # SF.selectWith (\x -> x < 3)

SF.selected xs -- Just 1
```

However, when using `mapSelected` (or `foldrSelected`) to map a function `f`
over a `SelectionFoldable` where the selected item appears more than once in the
list, `true` will be passed as the `IsSelected` argument for each instance of
the selected item:

```purescript
-- Here, both the first and last elements are equal to the selected item.
xs = SF.fromFoldable [1,2,1] # SF.select 1

mapSelected (\isSelected x -> if isSelected then "a" else "b") xs
-- (SelectionFoldableWithData ["a","b","a"] Just (Tuple unit "a")))
-- Note how both the first and last items were treated as if they were selected
```

## Developing

1. Install purescript: `npm install -g purescript`
2. Install  bower: `npm install -g bower`
3. Install pulp: `npm install -g pulp`
4. Install dependencies: `npm install && bower install`
5. Run tests: `pulp test`

## License

Licensed under a BSD 3-Clause license

## Documentation

- Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-selection-foldable).
