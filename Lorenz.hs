module Lorenz where

import Data.Default

data LzParam t = LzParam { sigma :: t, r :: t, b :: t } deriving (Eq, Show)

instance Floating t => Default (LzParam t) where
  def = LzParam 10 28 (8/3)

lorenz :: Floating t => t -> [t] -> [t]
lorenz = lorenzWith def

lorenzWith :: Floating t => LzParam t -> t -> [t] -> [t]
lorenzWith (LzParam sigma r b) _ [x, y, z] =
  [sigma * (y - x)
  , r * x - y - x * z
  , x * y - b * z]