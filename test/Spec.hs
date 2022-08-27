module Main
( main
)
where

import qualified Spec.Util
import qualified Test.Hspec.Runner      as Hspec


main :: IO ()
main = do
   Hspec.hspecWith Hspec.defaultConfig Spec.Util.spec
