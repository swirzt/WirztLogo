module Lib
  ( parserExp,
    parserComm,
    normalize,
    getTortu,
    rect,
    num2color,
    grad2radian,
    radian2grad,
    exp2Bound,
    exp2Bound',
    comm2Bound,
    comm2Bound',
  )
where

import Common
-- import Control.Monad (ap)
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import LogoPar

-- (.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .* g = \x y -> f (g x y)

parserExp :: String -> Maybe Exp
parserExp s =
  let ys = logo $ lexer s
   in either (\e -> Just e) (\_ -> Nothing) ys

parserComm :: String -> Maybe [Comm]
parserComm s =
  let ys = logo $ lexer s
   in either (\_ -> Nothing) (\c -> Just c) ys

exp2Bound :: Exp -> Exp
exp2Bound = exp2Bound' []

exp2Bound' :: [String] -> Exp -> Exp
exp2Bound' _ (Num n) = Num n
exp2Bound' v (Towards x y) = binary2Bound v x y Towards
exp2Bound' v e@(Var str) = maybe e Access $ elemIndex str v
exp2Bound' v (Sum e1 e2) = binary2Bound v e1 e2 Sum
exp2Bound' v (Difference e1 e2) = binary2Bound v e1 e2 Difference
exp2Bound' v (Multiply e1 e2) = binary2Bound v e1 e2 Multiply
exp2Bound' v (Divide e1 e2) = binary2Bound v e1 e2 Divide
exp2Bound' v (IfE e1 e2 e3) =
  let ee1 = exp2Bound' v e1
   in binary2Bound v e2 e3 (IfE ee1)
exp2Bound' _ (Access _) = undefined -- No debería llegar a esto, lo dejo para ver posibles errores
exp2Bound' v (Gt e1 e2) = binary2Bound v e1 e2 Gt
exp2Bound' v (Lt e1 e2) = binary2Bound v e1 e2 Lt
exp2Bound' v (Eq e1 e2) = binary2Bound v e1 e2 Eq
exp2Bound' v (GEq e1 e2) = binary2Bound v e1 e2 GEq
exp2Bound' v (LEq e1 e2) = binary2Bound v e1 e2 LEq
exp2Bound' v (Diff e1 e2) = binary2Bound v e1 e2 Diff
exp2Bound' v (And b1 b2) = binary2Bound v b1 b2 And
exp2Bound' v (Or b1 b2) = binary2Bound v b1 b2 Or
exp2Bound' v (Not b) = unary2Bound v b Not
exp2Bound' _ XCor = XCor
exp2Bound' _ YCor = YCor
exp2Bound' _ Heading = Heading
exp2Bound' _ Read = Read
exp2Bound' _ T = T
exp2Bound' _ F = F

-- exp2Bound' _ t = t -- Una vez que no haya problema reemplazar los ultimos con esto

binary2Bound :: [String] -> Exp -> Exp -> (Exp -> Exp -> g) -> g
binary2Bound v e1 e2 f =
  let ee1 = exp2Bound' v e1
      ee2 = exp2Bound' v e2
   in f ee1 ee2

unary2Bound :: [String] -> Exp -> (Exp -> g) -> g
unary2Bound v e f =
  let ee = exp2Bound' v e
   in f ee

comm2Bound :: Comm -> Comm
comm2Bound = comm2Bound' []

comm2Bound' :: [String] -> Comm -> Comm
comm2Bound' v (Ford e) = unary2Bound v e Ford
comm2Bound' v (Back e) = unary2Bound v e Back
comm2Bound' v (TRight e) = unary2Bound v e TRight
comm2Bound' v (TLeft e) = unary2Bound v e TLeft
comm2Bound' _ Clear = Clear
comm2Bound' _ Clean = Clean
comm2Bound' _ PUp = PUp
comm2Bound' _ PDown = PDown
comm2Bound' _ HideT = HideT
comm2Bound' _ ShowT = ShowT
comm2Bound' _ Home = Home
comm2Bound' v (SetX e) = unary2Bound v e SetX
comm2Bound' v (SetY e) = unary2Bound v e SetY
comm2Bound' v (SetXY e1 e2) = binary2Bound v e1 e2 SetXY
comm2Bound' v (SetHead e) = unary2Bound v e SetHead
comm2Bound' v (Rep e xs) =
  let ys = map (comm2Bound' v) xs
      ee = exp2Bound' v e
   in Rep ee ys
comm2Bound' v (Print e) = unary2Bound v e Print
comm2Bound' _ e@(PrintStr _) = e
comm2Bound' v (SetCo e) = unary2Bound v e SetCo
comm2Bound' v (IfC e xs ys) =
  let xs' = map (comm2Bound' v) xs
      ys' = map (comm2Bound' v) ys
      ee = exp2Bound' v e
   in IfC ee xs' ys'
comm2Bound' v (Def str ns cs) = Def str ns $ map (comm2Bound' (ns ++ v)) cs
comm2Bound' v (Save str e) = unary2Bound v e (Save str)
comm2Bound' v (ForDelta str e1 e2 e3 xs) =
  let v' = str : v
      ys = map (comm2Bound' v') xs
      ee1 = exp2Bound' v' e1
      ee2 = exp2Bound' v' e2
      ee3 = exp2Bound' v' e3
   in ForDelta str ee1 ee2 ee3 ys
comm2Bound' v (For str e1 e2 xs) =
  let v' = str : v
      ys = map (comm2Bound' v') xs
      ee1 = exp2Bound' v' e1
      ee2 = exp2Bound' v' e2
   in For str ee1 ee2 ys
comm2Bound' v (Wait e) = unary2Bound v e Wait
comm2Bound' v (While b xs) =
  let ys = map (comm2Bound' v) xs
      bb = exp2Bound' v b
   in While bb ys
comm2Bound' v (DoWhile xs b) =
  let ys = map (comm2Bound' v) xs
      bb = exp2Bound' v b
   in DoWhile ys bb
comm2Bound' v (CommVar str xs) =
  let ys = map (exp2Bound' v) xs
   in CommVar str ys
comm2Bound' _ Skip = Skip

-- expUnbound :: [Exp] -> Exp -> Exp
-- expUnbound _ (Num n) = Num n
-- expUnbound v (Towards x y) = binary2Bound v x y Towards
-- expUnbound v e@(Var str) = maybe e Access $ elemIndex str v
-- expUnbound v (Sum e1 e2) = binary2Bound v e1 e2 Sum
-- expUnbound v (Difference e1 e2) = binary2Bound v e1 e2 Difference
-- expUnbound v (Multiply e1 e2) = binary2Bound v e1 e2 Multiply
-- expUnbound v (Divide e1 e2) = binary2Bound v e1 e2 Divide
-- expUnbound v (IfE e1 e2 e3) =
--   let ee1 = expUnbound v e1
--    in binary2Bound v e2 e3 (IfE ee1)
-- expUnbound _ (Access _) = undefined -- No debería llegar a esto, lo dejo para ver posibles errores
-- expUnbound v (Gt e1 e2) = binary2Bound v e1 e2 Gt
-- expUnbound v (Lt e1 e2) = binary2Bound v e1 e2 Lt
-- expUnbound v (Eq e1 e2) = binary2Bound v e1 e2 Eq
-- expUnbound v (GEq e1 e2) = binary2Bound v e1 e2 GEq
-- expUnbound v (LEq e1 e2) = binary2Bound v e1 e2 LEq
-- expUnbound v (Diff e1 e2) = binary2Bound v e1 e2 Diff
-- expUnbound v (And b1 b2) = binary2Bound v b1 b2 And
-- expUnbound v (Or b1 b2) = binary2Bound v b1 b2 Or
-- expUnbound v (Not b) = unary2Bound v b Not
-- expUnbound _ XCor = XCor
-- expUnbound _ YCor = YCor
-- expUnbound _ Heading = Heading
-- expUnbound _ Read = Read
-- expUnbound _ T = T
-- expUnbound _ F = F

-- data Either3 a b c = Izq a | Medio b | Der c

-- instance Functor (Either3 a b) where
--   fmap f c = c >>= return . f

-- instance Applicative (Either3 a b) where
--   pure = return
--   (<*>) = ap

-- instance Monad (Either3 a b) where
--   return = Der
--   Izq a >>= _ = Izq a
--   Medio b >>= _ = Medio b
--   Der c >>= f = f c

-- either3 :: (a -> d) -> (b -> d) -> (c -> d) -> Either3 a b c -> d
-- either3 f _ _ (Izq a) = f a
-- either3 _ g _ (Medio b) = g b
-- either3 _ _ h (Der c) = h c

-- map3 :: (a -> d) -> (b -> e) -> (c -> f) -> Either3 a b c -> Either3 d e f
-- map3 f _ _ (Izq a) = Izq $ f a
-- map3 _ g _ (Medio b) = Medio $ g b
-- map3 _ _ h (Der c) = Der $ h c

dospi :: Float
dospi = 2 * pi

grad2radian :: Float -> Float
grad2radian = degToRad

radian2grad :: Float -> Float
radian2grad = radToDeg

-- repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a
-- repeatUntil c f x
--   | c x = x
--   | otherwise = repeatUntil c f (f x)

-- Mantiene el ángulo entre 0 y 2pi
normalize :: Float -> Float
normalize = normalizeAngle

rect :: Float -> Float -> Picture
rect x y = polygon $ rectanglePath x y

-- Esta tortuga mira por defecto para la derecha ya que es lo que le corresponde al angulo 0
tortuga :: Picture
tortuga =
  color green $
    pictures
      [ rect 30 40, -- cuerpo tortuga
        translate 0 24 $ rect 7 8, -- cabeza
        translate 18 15 $ rect 6 6, -- pata delantera derecha
        translate 18 (-15) $ rect 6 6, -- pata delantera izquierda
        translate (-18) 15 $ rect 6 6, -- pata trasera derecha
        translate (-18) (-15) $ rect 6 6 -- pata trasera izquierda
      ]

getTortu :: Float -> Float -> Float -> Picture
getTortu x y n = translate x y $ rotate n tortuga

customColor :: Float -> Float -> Float -> Color
customColor r g b = makeColor (r / 255) (g / 255) (b / 255) 1

num2color :: Int -> Color
num2color i = case i of
  0 -> black
  1 -> blue
  2 -> customColor 0 255 0
  3 -> cyan
  4 -> red
  5 -> magenta
  6 -> yellow
  7 -> white
  8 -> customColor 165 42 42
  9 -> customColor 210 180 140
  10 -> green
  11 -> aquamarine
  12 -> customColor 250 128 114
  13 -> customColor 128 0 128
  14 -> orange
  15 -> customColor 128 128 128
  _ -> undefined