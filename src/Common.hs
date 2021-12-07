{-# LANGUAGE GADTs #-}

module Common where

import Graphics.Gloss

data List a = Pos | L [Either (List a) (Exp a)] deriving (Show)

data Vars = S String | B Int

data Exp a
  = Str String
  | Num Float
  | XCor
  | YCor
  | Heading
  | Towards (Exp a)
  | Var a
  | Sum (Exp a) (Exp a)
  | Difference (Exp a) (Exp a)
  | Multiply (Exp a) (Exp a)
  | Divide (Exp a) (Exp a)
  | First (Exp a)
  | Last (Exp a)
  | Item (Exp a) (Exp a)
  | RandItem (Exp a)
  | Tail (Exp a)
  | RTail (Exp a)
  | Read
  | EList (List a)
  | IfE (Boolen a) (List a)
  deriving (Show)

data Boolen a
  = Gt (Exp a) (Exp a)
  | Lt (Exp a) (Exp a)
  | Eq (Exp a) (Exp a)
  | GEq (Exp a) (Exp a)
  | LEq (Exp a) (Exp a)
  | Diff (Exp a) (Exp a)
  | And (Boolen a) (Boolen a)
  | Or (Boolen a) (Boolen a)
  | Not (Boolen a)
  deriving (Show)

data Comm a
  = Ford (Exp a)
  | Back (Exp a)
  | TRight (Exp a)
  | TLeft (Exp a)
  | Clear
  | Clean
  | PUp
  | PDown
  | HideT
  | ShowT
  | Home
  | SetX (Exp a)
  | SetY (Exp a)
  | SetXY (Exp a) (Exp a)
  | SetHead (Exp a)
  | Rep (Exp a) [Comm a]
  | Print (Exp a)
  | Def String [String] [Comm a]
  | SetCo (Exp a)
  | DefV String (Exp a)
  | ForDelta String (Exp a) (Exp a) (Exp a) [Comm a]
  | For String (Exp a) (Exp a) [Comm a]
  | IfC (Boolen a) [Comm a]
  | Fill
  | Filled (Exp a) [Comm a]
  | Wait (Exp a)
  | While [Comm a] (Boolen a)
  | CommVar String (List a)
  deriving (Show)

rect :: Float -> Float -> Picture
rect x y = polygon $ rectanglePath x y

-- Esta tortuga mira por defecto para la derecha ya que es lo que le corresponde al angulo 0
tortuga :: Picture
tortuga =
  color green $
    pictures
      [ rect 40 30, -- cuerpo tortuga
        translate 24 0 $ rect 8 7, -- cabeza
        translate 15 18 $ rect 6 6, -- pata delantera derecha
        translate (-15) 18 $ rect 6 6, -- pata delantera izquierda
        translate 15 (-18) $ rect 6 6, -- pata trasera derecha
        translate (-15) (-18) $ rect 6 6 -- pata trasera izquierda
      ]

getTortu :: Float -> Float -> Float -> Picture
getTortu x y n = translate x y $ rotate n tortuga
