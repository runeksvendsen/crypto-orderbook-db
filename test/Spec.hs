module Main
( main
)
where

import qualified Spec.Util
import qualified Test.Hspec.Runner      as Hspec


main = do
   Hspec.hspecWith Hspec.defaultConfig Spec.Util.spec
