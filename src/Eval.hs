module Eval where

import Common
import Lib
import MonadLogo
import Graphics.Gloss
import System.Random
import GHC.Float.RealFracMethods
import GlobalEnv
import Data.Char

moveLine :: MonadLogo m => Exp -> (Float -> Float -> Float) -> m ()
moveLine exp f = do
  n <- runExpNum exp
  x <- getX
  y <- getY
  a <- getData dir
  let x' = f x ((cos a) * n)
      y' = f y ((sin a) * n)
      p = [(x,y), (x',y')]
      l = line p
  addPicture l
  setX x'
  setY y'

moveTo :: MonadLogo m => Float -> Float -> m ()
moveTo x y = do
  xc <- getX
  yc <- getY
  let p = [(x,y), (xc,yc)]
      l = line p
  addPicture l
  setX x
  setY y

setTo :: MonadLogo m => m Float -> m Float -> m ()
setTo f g = do
            x <- f
            y <- g
            moveTo x y

modAng :: MonadLogo m => Exp -> (Float -> Float -> Float) -> m ()
modAng exp f = do
  n <- runExpNum exp
  let radN = grad2radian n
  changeAng f radN

repeatComm :: MonadLogo m => Exp -> Comm -> m ()
repeatComm exp com = do
  n <- runExpNum exp
  let ni = floorFloatInt n
  if ni >= 0
     then repeatComm' ni com
     else failLogo "No se puede repetir un comando negativas veces."

repeatComm' :: MonadLogo m => Int -> Comm -> m ()
repeatComm' 0 _ = nada
repeatComm' n com = eval com >> repeatComm' (n-1) com

eval :: MonadLogo m => Comm -> m ()
eval (Ford exp) = moveLine exp (+)
eval (Back exp) = moveLine exp (-)
eval (TRight exp) = modAng exp (-)
eval (TLeft exp) = modAng exp (+)
eval Clear = setX 0 >> setY 0 >> setAng (pi/2) >> resetPics
eval Clean = resetPics
eval PUp = penUp
eval PDown = penDn
eval HideT = hideT
eval ShowT = showT
eval Home = moveTo 0 0
eval (SetX exp) = setTo (runExpNum exp) getY
eval (SetY exp) = setTo getX (runExpNum exp)
eval (SetXY exp1 exp2) = setTo (runExpNum exp1) (runExpNum exp2)
eval (SetHead exp) = modAng exp (\_ x -> x)
eval (Rep exp com) = repeatComm exp com
eval (Print exp) = do
  n <- runExpNum exp
  printLogo $ Prelude.show n
  -- | Def String [String] Comm
  -- | SetCo Exp
eval (DefV str exp) = newVar str exp
  -- | ForDelta String Exp Exp Exp Comm
  -- | For String Exp Exp Comm
  -- | If Boolen Comm
  -- | Fill
  -- | Filled Exp Comm
  -- | Wait Exp
  -- | While Comm Boolen

runEq :: MonadLogo m => Exp -> Exp -> (Float -> Float -> Bool) -> m Bool
runEq x y f = do
  x' <- runExpNum x 
  y' <- runExpNum y
  return (f x' y')

runBool :: MonadLogo m => Boolen -> Boolen -> (Bool -> Bool -> Bool) -> m Bool
runBool x y f = do
  x' <- runBoolen x
  y' <- runBoolen y
  return (f x' y')

runBoolen :: MonadLogo m => Boolen -> m Bool
runBoolen (Gt x y) = runEq x y (>)
runBoolen (Lt x y) = runEq x y (<)
runBoolen (Eq x y) = runEq x y (==)
runBoolen (GEq x y) = runEq x y (>=)
runBoolen (LEq x y) = runEq x y (<=)
runBoolen (Diff x y) = runEq x y (/=)
runBoolen (And x y) = runBool x y (&&)
runBoolen (Or x y) = runBool x y (||)
runBoolen (Not x) = runBool x x (\y _ -> not y)

binary :: MonadLogo m => Exp -> Exp -> (Float -> Float -> Float) -> m (Either List Float)
binary x y f = do
  x' <- runExp x
  y' <- runExp y
  let r = x' >>= \xi -> y' >>= \yi -> Right $ f xi yi
  case r of
    Right r' -> return $ Right r'
    _ -> failLogo "Error de tipos en operación binaria"

getRandom :: MonadLogo m => Int -> m Int
getRandom n = do
  g <- initStdGen
  let (x, _) = randomR (0, n -1) g
  return x

runExpNum :: MonadLogo m => Exp -> m Float
runExpNum x = do
  y <- runExp x
  case y of
    Right k -> return k
    Left _ -> failLogo "Se esperaba un número pero se obtuvo una lista"

runExp :: MonadLogo m => Exp -> m (Either3 List String Float)
runExp (Num n) = return $ Der n
runExp XCor = getX >>= return . Der
runExp YCor = getY >>= return . Der
runExp Heading = getData dir >>= return . Der . radian2grad
runExp (Towards l) = do x <- getX
                        y <- getY
                        e <- runExp l
                        case e of
  Izq ll -> let ln = listLength ll
            in if ln /= 2
               then failLogo $ "El comando towards espera una lista de tamaño 2, pero encontró una de tamaño: " ++ show ln ++ "."
               else 
  Medio _ -> failLogo "El comando towards esperaba una lista, pero se le dió un string."
  Der _ -> failLogo "El comando towards esperaba una lista, pero se le dió un número."
runExp (Var _) = undefined -- Monadico
runExp (Sum x y) = binary x y (+)
runExp (Difference x y) = binary x y (-)
runExp (Multiply x y) = binary x y (*)
runExp (Divide x y) = binary x y (/)
runExp (First xs) = listFuncList2 xs head
runExp (Last xs) = listFuncList2 xs fin
runExp (Item x xs) = do
  x' <- runExpNum x
  let xx = floorFloatInt x'
  listFuncElem xs xx
runExp (RandItem xs) = do
  let ll = listLength xs
  x <- getRandom ll
  listFuncElem xs x
runExp (Tail xs) = listFuncList xs tail
runExp (RTail xs) = listFuncList xs rTail
runExp Read = do
  input <- getLogo
  let input' = if isAlpha $ head input -- Si es un string le agrego " para que se pueda parsear
               then '"' : input
               else input
      e = parserExp input
  runExp e
runExp (EList l) = return $ Izq l

runFull :: MonadLogo m => Either List Exp -> m (Either List Float)
runFull (Right e) = runExp e
runFull (Left l) = return $ Left l

fin :: [a] -> a
fin [x] = x
fin (x : xs) = fin xs

rTail :: [a] -> [a]
rTail [x] = []
rTail (x : xs) = x : rTail xs

listLength :: List -> Int
listLength (L xs) = length xs

listFuncElem :: MonadLogo m => List -> Int -> m (Either List Float)
listFuncElem l x = do
  xs <- runList l
  runFull (xs !! x)

listFuncList :: MonadLogo m => List -> ([Either List Exp] -> [Either List Exp]) -> m (Either List Float)
listFuncList l f = do
  xs <- runList l
  let y = f xs
  return $ (Left . L) y

listFuncList2 :: MonadLogo m => List -> ([Either List Exp] -> Either List Exp) -> m (Either List Float)
listFuncList2 l f = do
  xs <- runList l
  runFull $ f xs

runList :: MonadLogo m => List -> m [Either List Exp]
runList Pos = do
  x <- getX
  y <- getY
  return $ map (Right . Num) [x, y]
runList (L xs) = return xs

