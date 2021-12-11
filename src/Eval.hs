module Eval where

import Common (Comm (..), Exp (..))
import Data.Functor ((<&>))
import GHC.Float.RealFracMethods (floorFloatInt)
import Graphics.Gloss (line)
import Lib (comm2Bound, grad2radian, parserExp, radian2grad)
import MonadLogo
import Relude.List ((!!?))

type EvalRet = Maybe [([Exp], Comm)]

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

moveLine :: MonadLogo m => [Exp] -> Exp -> (Float -> Float -> Float) -> m EvalRet
moveLine e expp f = do
  n <- runExp e expp
  x <- getX
  y <- getY
  a <- getDir
  let x' = f x (sin a * n) -- Cambio las trigonométricas por como se presenta el plano cartesiano
      y' = f y (cos a * n)
  moveTo x' y'

setTo :: MonadLogo m => m Float -> m Float -> m EvalRet
setTo f g = do
  x <- f
  y <- g
  moveTo x y

modDir :: MonadLogo m => [Exp] -> Exp -> (Float -> m ()) -> m EvalRet
modDir e expp f = do
  n <- runExp e expp
  let radN = grad2radian n
  f radN
  noth

repeatComm :: MonadLogo m => [Exp] -> Exp -> [Comm] -> m EvalRet
repeatComm e expp com = do
  n <- runExp e expp
  let ni = floorFloatInt n
  if ni >= 0
    then repeatComm' e ni com
    else failLogo "No se puede repetir un comando negativas veces."

conEnd :: [a] -> a -> [a]
conEnd ys x = foldr (:) [x] ys

mapCom :: [Exp] -> [Comm] -> Maybe [([Exp], Comm)]
mapCom e = return . map (\c -> (e, c))

repeatComm' :: MonadLogo m => [Exp] -> Int -> [Comm] -> m EvalRet
repeatComm' _ 0 _ = noth
repeatComm' e n com =
  let fN = fromIntegral (n - 1)
      rep = Rep (Num fN) com
      toret = mapCom e $ conEnd com rep
   in return toret

ifF :: Float -> Bool
ifF = (/= 0)

noth :: MonadLogo m => m EvalRet
noth = return Nothing

eval :: MonadLogo m => [Exp] -> Comm -> m EvalRet
eval e (Ford expp) = moveLine e expp (+)
eval e (Back expp) = moveLine e expp (-)
eval e (TRight expp) = modDir e expp (changeDir (+))
eval e (TLeft expp) = modDir e expp (changeDir (-))
eval _ Clear = setX 0 >> setY 0 >> setDir 0 >> resetPics >> noth
eval _ Clean = resetPics >> noth
eval _ PUp = penUp >> noth
eval _ PDown = penDn >> noth
eval _ HideT = hideT >> noth
eval _ ShowT = showT >> noth
eval _ Home = moveTo 0 0
eval e (SetX expp) = setTo (runExp e expp) getY
eval e (SetY expp) = setTo getX (runExp e expp)
eval e (SetXY exp1 exp2) = setTo (runExp e exp1) (runExp e exp2)
eval e (SetHead expp) = modDir e expp setDir
eval e (Rep expp com) = repeatComm e expp com
eval e (Print expp) = runExp e expp >>= printLogo . Prelude.show >> noth
eval _ (PrintStr str) = printLogo str >> noth
eval e (SetCo c) = do
  f <- runExp e c
  let n = floorFloatInt f
  if n >= 0 && n <= 15
    then setColor n >> noth
    else failLogo "Se quizo cambiar el color a un número fuera de rango, los colores van del 0 al 15"
eval e (IfC eb xs ys) = do
  b <- runExp e eb
  if ifF b
    then return $ mapCom e xs
    else return $ mapCom e ys
eval _ (Def name ns cs) = newComm name (length ns) (map (comm2Bound ns) cs) >> noth
eval e (Save str expp) = replace e expp >>= newVar str >> noth
eval e (For str initt end xs) = forLoop e initt end xs (+ 1) (\e1 e2 -> For str e1 e2 xs)
eval e (ForDelta str initt end delta xs) = do
  fdelta <- runExp e delta
  forLoop e initt end xs (+ fdelta) (\e1 e2 -> ForDelta str e1 e2 (Num fdelta) xs)
eval e (Wait expp) = runExp e expp >>= wait . floorFloatInt >> noth
eval e c@(While expp cs) = do
  b <- runExp e expp
  if ifF b
    then return $ fmap (++ [(e, c)]) (mapCom e cs)
    else noth
eval e (DoWhile cs expp) =
  let newWhile = While expp cs
   in return $ fmap (++ [(e, newWhile)]) (mapCom e cs)
eval e (CommVar str es) = do
  (n, com) <- getComm str
  if n /= length es
    then failLogo $ "La cantidad de argumentos que recibe la función: " ++ str ++ " es incorrecta."
    else return $ mapCom (es ++ e) com
eval _ Skip = noth

forLoop :: MonadLogo m => [Exp] -> Exp -> Exp -> [Comm] -> (Float -> Float) -> (Exp -> Exp -> Comm) -> m EvalRet
forLoop e e1 e2 cs f g = do
  ee1 <- runExp e e1
  ee2 <- runExp e e2
  if ee1 <= ee2
    then
      let newComm = g (Num (f ee1)) (Num ee2) -- Lo hago así para no recalcular
          ys = mapCom (e1 : e) cs
       in return $ fmap (++ [(e, newComm)]) ys
    else noth

binary :: MonadLogo m => [Exp] -> Exp -> Exp -> (Float -> Float -> a) -> m a
binary e x y f = do
  x' <- runExp e x
  y' <- runExp e y
  return $ f x' y'

bool2Float :: MonadLogo m => Bool -> m Float
bool2Float True = return 1
bool2Float False = return 0

binaryReplace :: MonadLogo m => [Exp] -> Exp -> Exp -> (Exp -> Exp -> g) -> m g
binaryReplace e e1 e2 f = do
  ee1 <- replace e e1
  ee2 <- replace e e2
  return $ f ee1 ee2

replace :: MonadLogo m => [Exp] -> Exp -> m Exp
replace e (Towards e1 e2) = binaryReplace e e1 e2 Towards
replace e (Var n) = getVar n >>= replace e
replace e (Sum x y) = binaryReplace e x y Sum
replace e (Difference x y) = binaryReplace e x y Difference
replace e (Multiply x y) = binaryReplace e x y Multiply
replace e (Divide x y) = binaryReplace e x y Divide
replace e (IfE eb et ef) = do
  eeb <- replace e eb
  eet <- replace e et
  eef <- replace e ef
  return $ IfE eeb eet eef
-- replace e (Access i) = replace e $ e !! i
replace e (Gt e1 e2) = binaryReplace e e1 e2 Gt
replace e (Lt e1 e2) = binaryReplace e e1 e2 Lt
replace e (Eq e1 e2) = binaryReplace e e1 e2 Eq
replace e (GEq e1 e2) = binaryReplace e e1 e2 GEq
replace e (LEq e1 e2) = binaryReplace e e1 e2 LEq
replace e (Diff e1 e2) = binaryReplace e e1 e2 Diff
replace e (And e1 e2) = binaryReplace e e1 e2 And
replace e (Or e1 e2) = binaryReplace e e1 e2 Or
replace e (Not expp) = replace e expp <&> Not
replace _ t = return t

runExp :: MonadLogo m => [Exp] -> Exp -> m Float
runExp _ (Num n) = return n
runExp _ XCor = getX
runExp _ YCor = getY
runExp _ Heading = getDir <&> radian2grad
runExp _ (Towards _ _) =
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
  if ifF b
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
runExp e (And e1 e2) = binary e e1 e2 (\x y -> ifF x && ifF y) >>= bool2Float
runExp e (Or e1 e2) = binary e e1 e2 (\x y -> ifF x || ifF y) >>= bool2Float
runExp e (Not expp) = runExp e expp >>= \x -> if ifF x then return 0 else return 1
runExp _ T = return 1
runExp _ F = return 0