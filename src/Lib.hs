module Lib
  ( someFunc,
    parserExp,
    parserComm,
  )
where

import Common
import Control.Applicative
import Control.Monad (ap)
import Data.Bifunctor
import Data.List
import Graphics.Gloss
import LogoPar

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .* g = \x y -> f (g x y)

parserExp :: String -> Maybe (Exp String)
parserExp s =
  let ys = logo $ lexer s
   in either (\e -> Just e) (\_ -> Nothing) ys

parserComm :: String -> Maybe [Comm String]
parserComm s =
  let ys = logo $ lexer s
   in either (\_ -> Nothing) (\c -> Just c) ys

expStr2Bound :: Exp String -> Exp Vars
expStr2Bound = expStr2Bound' []

expStr2Bound' :: [String] -> Exp String -> Exp Vars
expStr2Bound' _ (Str str) = Str str
expStr2Bound' _ (Num n) = Num n
expStr2Bound' _ XCor = XCor
expStr2Bound' _ YCor = YCor
expStr2Bound' _ Heading = Heading
expStr2Bound' v (Towards l) = unary2Bound v l Towards
expStr2Bound' v (Var str) = maybe (Var $ S str) (Var . B) $ elemIndex str v
expStr2Bound' v (Sum e1 e2) = binary2Bound v e1 e2 Sum
expStr2Bound' v (Difference e1 e2) = binary2Bound v e1 e2 Difference
expStr2Bound' v (Multiply e1 e2) = binary2Bound v e1 e2 Multiply
expStr2Bound' v (Divide e1 e2) = binary2Bound v e1 e2 Divide
expStr2Bound' v (First l) = unary2Bound v l First
expStr2Bound' v (Last l) = unary2Bound v l Last
expStr2Bound' v (Item l e) = binary2Bound v l e Item
expStr2Bound' v (RandItem l) = unary2Bound v l RandItem
expStr2Bound' v (Tail l) = unary2Bound v l Tail
expStr2Bound' v (RTail l) = unary2Bound v l RTail
expStr2Bound' _ Read = Read
expStr2Bound' v (EList l) = EList $ listStr2Bound' v l

binary2Bound :: [String] -> Exp String -> Exp String -> (Exp Vars -> Exp Vars -> g Vars) -> g Vars
binary2Bound v e1 e2 f =
  let ee1 = expStr2Bound' v e1
      ee2 = expStr2Bound' v e2
   in f ee1 ee2

unary2Bound :: [String] -> Exp String -> (Exp Vars -> g Vars) -> g Vars
unary2Bound v e f =
  let ee = expStr2Bound' v e
   in f ee

listStr2Bound :: List String -> List Vars
listStr2Bound = listStr2Bound' []

listStr2Bound' :: [String] -> List String -> List Vars
listStr2Bound' v Pos = Pos
listStr2Bound' v (L xs) = L $ map (bimap (listStr2Bound' v) (expStr2Bound' v)) xs

boolStr2Bound :: Boolen String -> Boolen Vars
boolStr2Bound = boolStr2Bound' []

boolStr2Bound' :: [String] -> Boolen String -> Boolen Vars
boolStr2Bound' v (Gt e1 e2) = binary2Bound v e1 e2 Gt
boolStr2Bound' v (Lt e1 e2) = binary2Bound v e1 e2 Lt
boolStr2Bound' v (Eq e1 e2) = binary2Bound v e1 e2 Eq
boolStr2Bound' v (GEq e1 e2) = binary2Bound v e1 e2 GEq
boolStr2Bound' v (LEq e1 e2) = binary2Bound v e1 e2 LEq
boolStr2Bound' v (Diff e1 e2) = binary2Bound v e1 e2 Diff
boolStr2Bound' v (And b1 b2) = bool2Bound v b1 b2 And
boolStr2Bound' v (Or b1 b2) = bool2Bound v b1 b2 Or
boolStr2Bound' v (Not b) = bool2Bound v b b (Not .* const)

bool2Bound :: [String] -> Boolen String -> Boolen String -> (Boolen Vars -> Boolen Vars -> g Vars) -> g Vars
bool2Bound v e1 e2 f =
  let ee1 = boolStr2Bound' v e1
      ee2 = boolStr2Bound' v e2
   in f ee1 ee2

commStr2Bound :: Comm String -> Comm Vars
commStr2Bound = commStr2Bound' []

commStr2Bound' :: [String] -> Comm String -> Comm Vars
commStr2Bound' v (Ford e) = unary2Bound v e Ford
commStr2Bound' v (Back e) = unary2Bound v e Back
commStr2Bound' v (TRight e) = unary2Bound v e TRight
commStr2Bound' v (TLeft e) = unary2Bound v e TLeft
commStr2Bound' _ Clear = Clear
commStr2Bound' _ Clean = Clean
commStr2Bound' _ PUp = PUp
commStr2Bound' _ PDown = PDown
commStr2Bound' _ HideT = HideT
commStr2Bound' _ ShowT = ShowT
commStr2Bound' _ Home = Home
commStr2Bound' v (SetX e) = unary2Bound v e SetX
commStr2Bound' v (SetY e) = unary2Bound v e SetY
commStr2Bound' v (SetXY e1 e2) = binary2Bound v e1 e2 SetXY
commStr2Bound' v (SetHead e) = unary2Bound v e SetHead
commStr2Bound' v (Rep e xs) =
  let ys = map (commStr2Bound' v) xs
      ee = expStr2Bound' v e
   in Rep ee ys
commStr2Bound' v (Print e) = unary2Bound v e Print
commStr2Bound' v (Def str xsS xsC) = Def str xsS $ map (commStr2Bound' (xsS ++ v)) xsC
commStr2Bound' v (SetCo e) = unary2Bound v e SetCo
commStr2Bound' v (DefV str e) = unary2Bound v e (DefV str)
commStr2Bound' v (ForDelta str e1 e2 e3 xs) =
  let ys = map (commStr2Bound' v) xs
      ee1 = expStr2Bound' v e1
      ee2 = expStr2Bound' v e2
      ee3 = expStr2Bound' v e3
   in ForDelta str ee1 ee2 ee3 ys
commStr2Bound' v (For str e1 e2 xs) =
  let ys = map (commStr2Bound' v) xs
      f = \x y -> For str x y ys
   in binary2Bound v e1 e2 f
commStr2Bound' v (If b xs) =
  let ys = map (commStr2Bound' v) xs
      f = \x _ -> If x ys
   in bool2Bound v b b f
commStr2Bound' _ Fill = Fill
commStr2Bound' v (Filled e xs) =
  let ys = map (commStr2Bound' v) xs
      f = \x -> Filled x ys
   in unary2Bound v e f
commStr2Bound' v (Wait e) = unary2Bound v e Wait
commStr2Bound' v (While xs b) =
  let ys = map (commStr2Bound' v) xs
   in bool2Bound v b b (While ys .* const)
commStr2Bound' v (CommVar str xs) =
  let ys = listStr2Bound' v xs
   in CommVar str ys

data Either3 a b c = Izq a | Medio b | Der c

instance Functor (Either3 a b) where
  fmap f c = c >>= return . f

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

someFunc :: IO ()
someFunc = undefined