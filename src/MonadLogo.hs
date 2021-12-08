{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Me permiten hacer la definicion e instancia de mi mónada

module MonadLogo where

import Common
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import GlobalEnv
import Graphics.Gloss
import Lib
import System.IO

class (MonadIO m, MonadState Env m, MonadError String m) => MonadLogo m

nada :: MonadLogo m => m ()
nada = return ()

failLogo :: MonadLogo m => String -> m a
failLogo str = throwError str

getLogo :: MonadLogo m => m String
getLogo = liftIO $ getLine

printLogo :: MonadLogo m => String -> m ()
printLogo = liftIO . putStrLn

getInput :: MonadLogo m => String -> m String
getInput i = (liftIO $ putStr i) >> getLogo

printGraph :: MonadLogo m => m ()
printGraph = do
  Env x y ang b _ _ pic _ _ d <- get
  let tortu = getTortu x y ang
      paraPrint =
        if b
          then tortu : pic
          else pic
  liftIO $ display d white (pictures paraPrint)

getData :: MonadLogo m => (Env -> a) -> m a
getData f = get >>= return . f

getX :: MonadLogo m => m Float
getX = getData posx

getY :: MonadLogo m => m Float
getY = getData posy

getDir :: MonadLogo m => m Float
getDir = getData dir

addPicture :: MonadLogo m => Picture -> m ()
addPicture p = do
  b <- getData pen
  c <- getData GlobalEnv.color
  let pp = Color c p
  if b
    then nada
    else modify (\s -> s {pics = pp : pics s})

resetPics :: MonadLogo m => m ()
resetPics = modify (\s -> s {pics = []})

setX :: MonadLogo m => Float -> m ()
setX n = modify (\s -> s {posx = n})

changeX :: MonadLogo m => (Float -> Float -> Float) -> Float -> m ()
changeX f n = getData posx >>= \x -> setX (f x n)

setY :: MonadLogo m => Float -> m ()
setY n = modify (\s -> s {posy = n})

changeY :: MonadLogo m => (Float -> Float -> Float) -> Float -> m ()
changeY f n = getData posy >>= \y -> setY (f y n)

setDir :: MonadLogo m => Float -> m ()
setDir n = modify (\s -> s {dir = n})

changeDir :: MonadLogo m => (Float -> Float -> Float) -> Float -> m ()
changeDir f n = modify (\s -> s {dir = normalize (f (dir s) n)})

setColor :: MonadLogo m => Int -> m ()
setColor n = modify (\s -> s {GlobalEnv.color = num2color n})

showT :: MonadLogo m => m ()
showT = modify (\s -> s {GlobalEnv.show = True})

hideT :: MonadLogo m => m ()
hideT = modify (\s -> s {GlobalEnv.show = False})

penUp :: MonadLogo m => m ()
penUp = modify (\s -> s {pen = True})

penDn :: MonadLogo m => m ()
penDn = modify (\s -> s {pen = False})

newVar :: MonadLogo m => String -> Exp -> m ()
newVar str e = do
  varr <- getData vars
  if M.member str varr
    then failLogo ("Variable: " ++ str ++ ", ya declarada.")
    else modify (\s -> s {vars = M.insert str e varr})

delVar :: MonadLogo m => String -> m ()
delVar str = do
  varr <- getData vars
  if M.notMember str varr
    then failLogo ("Variable: " ++ str ++ ", no está definida, no se puede eliminar.")
    else modify (\s -> s {vars = M.delete str varr})

getVar :: MonadLogo m => String -> m Exp
getVar str = do
  varr <- getData vars
  let mv = M.lookup str varr
  case mv of
    Nothing -> failLogo ("Variable: " ++ str ++ ", no está definida, no se puede acceder.")
    Just e -> return e

newComm :: MonadLogo m => String -> Int -> [Comm] -> m ()
newComm str n c = do
  comm <- getData comms
  if M.member str comm
    then failLogo ("comando: " ++ str ++ ", ya declarado.")
    else modify (\s -> s {comms = M.insert str (n, c) comm})

delComm :: MonadLogo m => String -> m ()
delComm str = do
  comm <- getData comms
  if M.notMember str comm
    then failLogo ("Comando: " ++ str ++ ", no está definido, no se puede eliminar.")
    else modify (\s -> s {comms = M.delete str comm})

getComm :: MonadLogo m => String -> m (Int, [Comm])
getComm str = do
  comm <- getData comms
  let mv = M.lookup str comm
  case mv of
    Nothing -> failLogo ("Comando: " ++ str ++ ", no está definido, no se puede acceder.")
    Just c -> return c

wait :: MonadLogo m => Int -> m ()
wait = liftIO . threadDelay

catchErrors :: MonadLogo m => m a -> m (Maybe a)
catchErrors c =
  catchError
    (Just <$> c)
    ( \e ->
        liftIO $
          hPutStrLn stderr (Prelude.show e)
            >> return Nothing
    )

type Logo = StateT Env (ExceptT String IO)

instance MonadLogo Logo

runLogo :: Display -> Logo a -> IO (Either String (a, Env))
runLogo d m = runExceptT $ runStateT m $ defaultEnv d