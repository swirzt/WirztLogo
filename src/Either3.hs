module Either3 where

import Control.Monad (ap)
import Data.Functor ((<&>))

data Either3 a b c = Izq a | Medio b | Der c

instance Functor (Either3 a b) where
  fmap f c = c <&> f

instance Applicative (Either3 a b) where
  pure = return
  (<*>) = ap

instance Monad (Either3 a b) where
  return = Der
  Izq a >>= _ = Izq a
  Medio b >>= _ = Medio b
  Der c >>= f = f c

either3 :: (a -> d) -> (b -> d) -> (c -> d) -> Either3 a b c -> d
either3 f _ _ (Izq a) = f a
either3 _ g _ (Medio b) = g b
either3 _ _ h (Der c) = h c

map3 :: (a -> d) -> (b -> e) -> (c -> f) -> Either3 a b c -> Either3 d e f
map3 f _ _ (Izq a) = Izq $ f a
map3 _ g _ (Medio b) = Medio $ g b
map3 _ _ h (Der c) = Der $ h c