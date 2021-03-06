module Eval where

-- Imports de librerías
import           Data.Functor ((<&>))
import           GHC.Float.RealFracMethods (floorFloatInt)
import           Graphics.Gloss (arc, line, rotate, scale, text, translate)
import           Relude.List ((!!?))
import           Control.Monad (when)
--  Imports locales
import           Common (Comm(..), Exp(..))
import           Lib (parserExp, radian2grad, isReserved, getBoolOp, getNumOp, ifF)
import           MonadLogo

type EvalRet = [([Exp], Comm)]

moveTo :: MonadLogo m => Float -> Float -> m EvalRet
moveTo x y = do
  xc <- getX
  yc <- getY
  let p = [(x, y), (xc, yc)]
      l = line p
  addPicture l
  setX x
  setY y
  noth

moveLine
  :: MonadLogo m => [Exp] -> Exp -> (Float -> Float -> Float) -> m EvalRet
moveLine e expp f = do
  n <- runExp e expp
  x <- getX
  y <- getY
  a <- getDirRad
  let x' = f x (sin a * n) -- Cambio las trigonométricas por como se presenta el plano cartesiano
      y' = f y (cos a * n)
  moveTo x' y'

setTo :: MonadLogo m => m Float -> m Float -> m EvalRet
setTo f g = do
  x <- f
  y <- g
  moveTo x y

modDir :: MonadLogo m => [Exp] -> Exp -> (Float -> m ()) -> m EvalRet
modDir e expp f = runExp e expp >>= f >> noth

repeatComm :: MonadLogo m => [Exp] -> Exp -> [Comm] -> m EvalRet
repeatComm e expp com = do
  n <- runExp e expp
  let ni = floorFloatInt n
  if ni >= 0
    then repeatComm' e ni com
    else failLogo "No se puede repetir un comando negativas veces."

conEnd :: [a] -> a -> [a]
conEnd ys x = foldr (:) [x] ys

mapCom :: [Exp] -> [Comm] -> EvalRet
mapCom e = map (\c -> (e, c))

repeatComm' :: MonadLogo m => [Exp] -> Int -> [Comm] -> m EvalRet
repeatComm' _ 0 _ = noth
repeatComm' e n com = let fN = fromIntegral (n - 1)
                          rep = Rep (Num fN) com
                          toret = mapCom e $ conEnd com rep
                      in return toret

noth :: MonadLogo m => m EvalRet
noth = return []

eval :: MonadLogo m => [Exp] -> Comm -> m EvalRet
eval e (Ford expp) = moveLine e expp (+)
eval e (Back expp) = moveLine e expp (-)
eval e (TRight expp) = modDir e expp (changeDir (+))
eval e (TLeft expp) = modDir e expp (changeDir (-))
eval _ Clear = setX 0 >> setY 0 >> setDir 0 >> resetPics >> setColor 0 >> noth
eval _ Clean = resetPics >> noth
eval _ PUp = penUp >> noth
eval _ PDown = penDn >> noth
eval _ HideT = hideT >> noth
eval _ ShowT = showT >> noth
eval _ Home = setDir 0 >> moveTo 0 0
eval e (SetX expp) = setTo (runExp e expp) getY
eval e (SetY expp) = setTo getX (runExp e expp)
eval e (SetXY exp1 exp2) = setTo (runExp e exp1) (runExp e exp2)
eval e (SetHead expp) = modDir e expp setDir
eval e (Rep expp com) = repeatComm e expp com
eval e (Print expp) = runExp e expp >>= showLogo >> noth
eval _ (PrintStr str) = printLogo str >> noth
eval e (SetCo c) = do
  f <- runExp e c
  let n = floorFloatInt f
  if n >= 0 && n <= 15
    then setColor n >> noth
    else failLogo
      "Se quizo cambiar el color a un número fuera de rango, los colores van del 0 al 15"
eval e (SetCoCustom e1 e2 e3) = do
  r <- runExp e e1
  g <- runExp e e2
  b <- runExp e e3
  if all (\x -> x >= 0 && x <= 255) [r, g, b]
    then setCustomColor r g b >> noth
    else failLogo "Uno de los colores no está en el rango de [0,255]"
eval e (IfC eb xs ys) = do
  b <- runExp e eb
  if ifF b
    then return $ mapCom e xs
    else return $ mapCom e ys
eval _ (Def "" _ _) = failLogo "No se dió un nombre al comando a definir"
eval _ (Def name ns cs)
  | isReserved
    name = failLogo "El comando que se quiere definir es una palabra reservada"
  | otherwise = newComm name (length ns) cs >> noth
eval _ (Save "" _) = failLogo "No se dió un nombre a la variable a definir"
eval e (Save str expp) = runExp e expp >>= newVar str >> noth
eval _ (For "" _ _ _) = failLogo "No se dió un nombre a la variable del for"
eval e (For str initt end xs) = forLoop e initt end xs (+ 1) (For str)
eval _ (ForDelta "" _ _ _ _) =
  failLogo "No se dió un nombre a la variable del for"
eval e (ForDelta str initt end delta xs) = do
  fdelta <- runExp e delta
  forLoop e initt end xs (+ fdelta) (\e1 e2 -> ForDelta str e1 e2 (Num fdelta))
eval e (Wait expp) = runExp e expp >>= setWait >> noth
eval e c@(While expp cs) = do
  b <- runExp e expp
  if ifF b
    then return $ mapCom e cs ++ [(e, c)]
    else noth
eval e (DoWhile cs expp) = let newWhile = While expp cs
                           in return $ mapCom e cs ++ [(e, newWhile)]
eval e (CommVar str es) = do
  (n, com) <- getComm str
  newEs <- mapM (\x -> runExp e x <&> Num) es
  if n /= length es
    then failLogo
      $ "La cantidad de argumentos que recibe la función: "
      ++ str
      ++ " es incorrecta."
    else return $ mapCom (newEs ++ e) com
eval e (ChangeScale expp) = runExp e expp >>= setScale >> noth
eval e (Arco e1 e2) = do
  x <- getX
  y <- getY
  dir <- getDirDeg
  ee1 <- runExp e e1
  ee2 <- runExp e e2
  let degdir = 90 - dir
      todir = degdir - ee1
      (a1, a2) = if ee1 < 0
                 then (degdir, todir)
                 else (todir, degdir)
      a = translate x y $ arc a1 a2 ee2
  when (ifF ee1)
    $ addPicture a -- Si el ángulo es 0 no genero el arco, aprovecho lazyness
  noth
eval _ (Texto str) = do
  x <- getX
  y <- getY
  dir <- getDirDeg
  size <- getSizeT
  let ang = -90 + dir
      a = scale size size $ translate x y $ rotate ang $ text str
  addPicture a
  noth
eval e (SetSizeTexto expp) = runExp e expp >>= setSizeT . (/ 10) >> noth
eval _ Undo = undoPic >> noth
eval _ Skip = noth

forLoop :: MonadLogo m
        => [Exp]
        -> Exp
        -> Exp
        -> [Comm]
        -> (Float -> Float)
        -> (Exp -> Exp -> [Comm] -> Comm)
        -> m EvalRet
forLoop e e1 e2 cs f g = do
  ee1 <- runExp e e1
  ee2 <- runExp e e2
  if ee1 <= ee2
    then let newFor = g (Num (f ee1)) (Num ee2) cs -- Lo hago así para no recalcular
             ys = mapCom (Num ee1:e) cs
         in return $ ys ++ [(e, newFor)]
    else noth

binary :: MonadLogo m => [Exp] -> Exp -> Exp -> (Float -> Float -> a) -> m a
binary e x y f = do
  x' <- runExp e x
  y' <- runExp e y
  return $ f x' y'

bool2Float :: MonadLogo m => Bool -> m Float
bool2Float True = return 1
bool2Float False = return 0

cuadrante :: Float -> Float -> Float -> Float
cuadrante rad x y
  | x >= 0 && y >= 0 = rad
  | x >= 0 = rad + 360
  | otherwise = rad + 180

runExp :: MonadLogo m => [Exp] -> Exp -> m Float
runExp _ (Num n) = return n
runExp e (Negative expp) = runExp e expp >>= \x -> return (-x)
runExp _ XCor = getX
runExp _ YCor = getY
runExp _ Heading = getDirDeg
runExp e (Towards e1 e2) = do
  x <- getX
  y <- getY
  x' <- runExp e e1
  y' <- runExp e e2
  let vx = y' - y
      vy = x' - x
      tangente = vy / vx
      angle = atan tangente
      grad = if vx == 0
             then if vy >= 0
                  then 90
                  else 270
             else cuadrante (radian2grad angle) vx vy
  return grad
runExp _ (Var name) = getVar name
runExp e (BinaryOp f x y) = binary e x y (getNumOp f)
runExp e Read = do
  input <- getInput "Ingrese una exp"
  case parserExp input of
    Left err -> printLogo ("Ingreso incorrecto, ingrese nuevamente.\n" ++ err)
      >> runExp e Read
    Right i  -> runExp e i
runExp e (IfE eb et ef) = do
  b <- runExp e eb
  if ifF b
    then runExp e et
    else runExp e ef
runExp e (Access i) = case e !!? i of
  Just n  -> runExp e n
  Nothing -> failLogo "Se accedió a una variable no ligada por el entorno"
runExp e (Compare f e1 e2) = binary e e1 e2 (getBoolOp f) >>= bool2Float
runExp e (Not expp) = runExp e expp
  >>= \x -> if ifF x
            then return 0
            else return 1
runExp _ T = return 1
runExp _ F = return 0
runExp e (Random expp) = runExp e expp >>= getRandom
