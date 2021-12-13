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
    comm2Bound,
  )
where

import Common
import Data.List (elemIndex)
import Either3 (either3)
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle (degToRad, normalizeAngle, radToDeg)
import LogoPar (lexer, logo)

nothing :: a -> Maybe b
nothing = const Nothing

parserExp :: String -> Maybe Exp
parserExp s =
  let ys = logo $ lexer s
   in either3 nothing Just nothing ys

parserComm :: String -> Maybe [Comm]
parserComm s =
  let ys = logo $ lexer s
   in either3 nothing nothing Just ys

binaryOp :: (a -> b -> b) -> a -> b -> b -> (b -> b -> c) -> c
binaryOp f v e1 e2 g =
  let ee1 = f v e1
      ee2 = f v e2
   in g ee1 ee2

unaryOp :: (a -> b -> b) -> a -> b -> (b -> c) -> c
unaryOp f v e g = let ee = f v e in g ee

-- Recibe una lista de varibales locales y una Exp
-- Reemplaza los llamados a variable local por un Bound i
-- donde i es el indice de la variable en la lista
exp2Bound :: [String] -> Exp -> Exp
exp2Bound v (Negative e) = unary2Bound v e Negative
exp2Bound v (Towards x y) = binary2Bound v x y Towards
exp2Bound v e@(Var str) = maybe e Access $ elemIndex str v
exp2Bound v (BinaryOp f e1 e2) = binary2Bound v e1 e2 (BinaryOp f)
exp2Bound v (IfE e1 e2 e3) =
  let ee1 = exp2Bound v e1
   in binary2Bound v e2 e3 (IfE ee1)
exp2Bound v (Compare f e1 e2) = binary2Bound v e1 e2 (Compare f)
exp2Bound v (And b1 b2) = binary2Bound v b1 b2 And
exp2Bound v (Or b1 b2) = binary2Bound v b1 b2 Or
exp2Bound v (Not b) = unary2Bound v b Not
exp2Bound _ t = t -- Una vez que no haya problema reemplazar los ultimos con esto

binary2Bound :: [String] -> Exp -> Exp -> (Exp -> Exp -> g) -> g
binary2Bound = binaryOp exp2Bound

unary2Bound :: [String] -> Exp -> (Exp -> g) -> g
unary2Bound = unaryOp exp2Bound

comm2Bound :: [String] -> Comm -> Comm
comm2Bound v (Ford e) = unary2Bound v e Ford
comm2Bound v (Back e) = unary2Bound v e Back
comm2Bound v (TRight e) = unary2Bound v e TRight
comm2Bound v (TLeft e) = unary2Bound v e TLeft
comm2Bound v (SetX e) = unary2Bound v e SetX
comm2Bound v (SetY e) = unary2Bound v e SetY
comm2Bound v (SetXY e1 e2) = binary2Bound v e1 e2 SetXY
comm2Bound v (SetHead e) = unary2Bound v e SetHead
comm2Bound v (Rep e xs) =
  let ys = map (comm2Bound v) xs
      ee = exp2Bound v e
   in Rep ee ys
comm2Bound v (Print e) = unary2Bound v e Print
comm2Bound v (SetCo e) = unary2Bound v e SetCo
comm2Bound v (IfC e xs ys) =
  let xs' = map (comm2Bound v) xs
      ys' = map (comm2Bound v) ys
      ee = exp2Bound v e
   in IfC ee xs' ys'
comm2Bound v (Def str ns cs) = Def str ns $ map (comm2Bound (ns ++ v)) cs
comm2Bound v (Save str e) = unary2Bound v e (Save str)
comm2Bound v (ForDelta str e1 e2 e3 xs) =
  let v' = str : v
      ys = map (comm2Bound v') xs
      ee1 = exp2Bound v' e1
      ee2 = exp2Bound v' e2
      ee3 = exp2Bound v' e3
   in ForDelta str ee1 ee2 ee3 ys
comm2Bound v (For str e1 e2 xs) =
  let v' = str : v
      ys = map (comm2Bound v') xs
      ee1 = exp2Bound v' e1
      ee2 = exp2Bound v' e2
   in For str ee1 ee2 ys
comm2Bound v (Wait e) = unary2Bound v e Wait
comm2Bound v (While b xs) =
  let ys = map (comm2Bound v) xs
      bb = exp2Bound v b
   in While bb ys
comm2Bound v (DoWhile xs b) =
  let ys = map (comm2Bound v) xs
      bb = exp2Bound v b
   in DoWhile ys bb
comm2Bound v (CommVar str xs) =
  let ys = map (exp2Bound v) xs
   in CommVar str ys
comm2Bound _ c = c

grad2radian :: Float -> Float
grad2radian = degToRad

radian2grad :: Float -> Float
radian2grad = radToDeg

-- Mantiene el ángulo entre 0 y 2pi
normalize :: Float -> Float
normalize = normalizeAngle

rect :: Float -> Float -> Picture
rect x y = polygon $ rectanglePath x y

-- Esta tortuga mira por defecto hacia arriba ya que es lo que le corresponde al ángulo 0
tortuga :: Picture
tortuga =
  scale 0.1 0.1 $
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
getTortu x y n = translate x y $ rotate (radian2grad n) tortuga

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