module Eval where

import Common
import Data.Char
import GHC.Float.RealFracMethods
import GlobalEnv
import Graphics.Gloss
import Lib
import MonadLogo
import Relude.List

moveTo :: MonadLogo m => Float -> Float -> m ()
moveTo x y = do
  xc <- getX
  yc <- getY
  let p = [(x, y), (xc, yc)]
      l = line p
  addPicture l
  setX x
  setY y

moveLine :: MonadLogo m => [Exp] -> Exp -> (Float -> Float -> Float) -> m ()
moveLine e exp f = do
  n <- runExp e exp
  x <- getX
  y <- getY
  a <- getDir
  let x' = f x ((sin a) * n) -- Cambio las trigonométricas por como está ordenado esto
      y' = f y ((cos a) * n)
  moveTo x' y'

setTo :: MonadLogo m => m Float -> m Float -> m ()
setTo f g = do
  x <- f
  y <- g
  moveTo x y

modDir :: MonadLogo m => [Exp] -> Exp -> (Float -> m ()) -> m ()
modDir e exp f = do
  n <- runExp e exp
  let radN = grad2radian n
  f radN

repeatComm :: MonadLogo m => [Exp] -> Exp -> [Comm] -> m ()
repeatComm e exp com = do
  n <- runExp e exp
  let ni = floorFloatInt n
  if ni >= 0
    then repeatComm' e ni com
    else failLogo "No se puede repetir un comando negativas veces."

repeatComm' :: MonadLogo m => [Exp] -> Int -> [Comm] -> m ()
repeatComm' _ 0 _ = nada
repeatComm' e n com = evalList e com >> repeatComm' e (n - 1) com

evalList :: MonadLogo m => [Exp] -> [Comm] -> m ()
evalList _ [] = nada
evalList e (x : xs) = eval e x >> evalList e xs

ifFloat :: Float -> Bool
ifFloat = (/= 0)

eval :: MonadLogo m => [Exp] -> Comm -> m ()
eval e (Ford exp) = moveLine e exp (+)
eval e (Back exp) = moveLine e exp (-)
eval e (TRight exp) = modDir e exp (changeDir (+))
eval e (TLeft exp) = modDir e exp (changeDir (-))
eval _ Clear = setX 0 >> setY 0 >> setDir 0 >> resetPics
eval _ Clean = resetPics
eval _ PUp = penUp
eval _ PDown = penDn
eval _ HideT = hideT
eval _ ShowT = showT
eval _ Home = moveTo 0 0
eval e (SetX exp) = setTo (runExp e exp) getY
eval e (SetY exp) = setTo getX (runExp e exp)
eval e (SetXY exp1 exp2) = setTo (runExp e exp1) (runExp e exp2)
eval e (SetHead exp) = modDir e exp setDir
eval e (Rep exp com) = repeatComm e exp com
eval e (Print exp) = runExp e exp >>= printLogo . Prelude.show
eval _ (PrintStr str) = printLogo str
eval e (SetCo c) = do
  f <- runExp e c
  let n = floorFloatInt f
  if n >= 0 && n <= 15
    then setColor n
    else failLogo "Se quizo cambiar el color a un número fuera de rango, los colores van del 0 al 15"
eval e (IfC eb xs ys) = do
  b <- runExp e eb
  if ifFloat b
    then evalList e xs
    else evalList e ys
-- Asumo que ya se bindió antes de evaluar
eval _ (Def name ns cs) = newComm name (length ns) cs
eval _ (Save str exp) = newVar str exp
eval e (For _ init end xs) = forLoop e init end xs (+ 1)
eval e (ForDelta _ init end delta xs) = do
  delt <- runExp e delta
  forLoop e init end xs (+ delt)
eval e (Wait exp) = runExp e exp >>= wait . floorFloatInt
eval e (While exp cs) = do
  b <- runExp e exp
  if ifFloat b
    then evalList e cs >> eval e (While exp cs)
    else nada
eval e (DoWhile cs exp) = do
  evalList e cs
  b <- runExp e exp
  if ifFloat b
    then eval e (DoWhile cs exp)
    else nada
eval e (CommVar str es) = do
  (n, com) <- getComm str
  if n /= length es
    then failLogo $ "La cantidad de argumentos que recibe la función: " ++ str ++ " es incorrecta."
    else evalList (es ++ e) com
eval _ Skip = nada

forLoop :: MonadLogo m => [Exp] -> Exp -> Exp -> [Comm] -> (Float -> Float) -> m ()
forLoop e e1 e2 cs f = do
  ee1 <- runExp e e1
  ee2 <- runExp e e2
  forLoop' e ee1 ee2 cs f

forLoop' :: MonadLogo m => [Exp] -> Float -> Float -> [Comm] -> (Float -> Float) -> m ()
forLoop' e i n cs f
  | i < n = evalList (Num i : e) cs >> forLoop' e (f i) n cs f
  | otherwise = nada

binary :: MonadLogo m => [Exp] -> Exp -> Exp -> (Float -> Float -> a) -> m a
binary e x y f = do
  x' <- runExp e x
  y' <- runExp e y
  return $ f x' y'

bool2Float :: MonadLogo m => Bool -> m Float
bool2Float True = return 1
bool2Float False = return 0

runExp :: MonadLogo m => [Exp] -> Exp -> m Float
runExp _ (Num n) = return n
runExp _ XCor = getX
runExp _ YCor = getY
runExp _ Heading = getDir >>= return . radian2grad
runExp e (Towards xt yt) = do
  x <- getX
  y <- getY
  x' <- runExp e xt
  y' <- runExp e yt
  return 0 -- Temporal, no se resolverlo
runExp e (Var name) = getVar name >>= runExp e
runExp e (Sum x y) = binary e x y (+)
runExp e (Difference x y) = binary e x y (-)
runExp e (Multiply x y) = binary e x y (*)
runExp e (Divide x y) = binary e x y (/)
runExp e Read = do
  printLogo "Ingrese una expresión:\n>>"
  input <- getLogo
  case parserExp input of
    Nothing -> printLogo "Ingreso incorrecto, vuelva a intentar." >> runExp e Read
    Just i -> runExp e i
-- let input' = if isAlpha $ head input -- Si es un string le agrego " para que se pueda parsear
--              then ':' : input
--              else input
runExp e (IfE eb et ef) = do
  b <- runExp e eb
  if ifFloat b
    then runExp e et
    else runExp e ef
runExp e (Access i) = case e !!? i of
  Just n -> runExp e n
  Nothing -> failLogo "Se accedió a una variable no ligada por el entorno"
runExp e (Gt e1 e2) = binary e e1 e2 (>) >>= bool2Float
runExp e (Lt e1 e2) = binary e e1 e2 (<) >>= bool2Float
runExp e (Eq e1 e2) = binary e e1 e2 (==) >>= bool2Float
runExp e (GEq e1 e2) = binary e e1 e2 (>=) >>= bool2Float
runExp e (LEq e1 e2) = binary e e1 e2 (<=) >>= bool2Float
runExp e (Diff e1 e2) = binary e e1 e2 (/=) >>= bool2Float
runExp e (And e1 e2) = binary e e1 e2 (\x y -> ifFloat x && ifFloat y) >>= bool2Float
runExp e (Or e1 e2) = binary e e1 e2 (\x y -> ifFloat x || ifFloat y) >>= bool2Float
runExp e (Not exp) = runExp e exp >>= \x -> if ifFloat x then return 0 else return 1
runExp _ T = return 1
runExp _ F = return 0
