module Test.Main where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.SelectionFoldable as SelectionFoldable
import Test.SelectionFoldableWithData as SelectionFoldableWithData
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
    SelectionFoldableWithData.spec
    SelectionFoldable.spec
