module Test.Fluent.Diff where

import Data.Default (Default (def))
import qualified Data.Text as T
import qualified Pretty.Diff as Diff

pretty :: Show a => a -> a -> String
pretty a b =
  let x = T.pack $ show a
      y = T.pack $ show b
   in T.unpack $ Diff.pretty def x y
