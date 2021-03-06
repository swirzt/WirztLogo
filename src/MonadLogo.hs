{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MonadLogo where

-- Imports de librerías
import           Control.Concurrent (putMVar, takeMVar)
import           Control.Monad (when)
import           Control.Monad.Except (ExceptT, MonadError(..), MonadIO(..)
                                     , runExceptT)
import           Control.Monad.State (MonadState(get), StateT(runStateT)
                                    , modify)
import qualified Data.Map as M
import           Graphics.Gloss (Picture(Color))
import           Relude.Lifted.Exit (exitSuccess)
import           System.Random (randomR)
-- Imports locales
import           Common (Comm, Input(Exit, Input), Output(GetExp, Show))
import           GlobalEnv (Env(..))
import           Lib (normalizeRad, normalizeDeg, num2color, customColor
                    , grad2radian)

class (MonadIO m, MonadState Env m, MonadError String m) => MonadLogo m

nada :: MonadLogo m => m ()
nada = return ()

failLogo :: MonadLogo m => String -> m a
failLogo = throwError

showLogo :: (MonadLogo m, Show a) => a -> m ()
showLogo a = printLogo $ Prelude.show a

printLogo :: MonadLogo m => String -> m ()
printLogo str = do
  s <- get
  liftIO $ putMVar (out s) (Show str)

getInput :: MonadLogo m => String -> m String
getInput i = do
  s <- get
  liftIO (putMVar (out s) (Show i))
  liftIO (putMVar (out s) GetExp)
  input <- liftIO (takeMVar (inp s))
  case input of
    Exit      -> exitSuccess
    Input str -> return str
    _         -> undefined -- No debería llegar acá

getData :: MonadLogo m => (Env -> a) -> m a
getData f = get >>= return . f

getX :: MonadLogo m => m Float
getX = getData posx

getY :: MonadLogo m => m Float
getY = getData posy

getDirRad :: MonadLogo m => m Float
getDirRad = getData dirRad

getDirDeg :: MonadLogo m => m Float
getDirDeg = getData dirDeg

getSizeT :: MonadLogo m => m Float
getSizeT = getData sizeT

addPicture :: MonadLogo m => Picture -> m ()
addPicture p = do
  b <- getData pen
  c <- getData GlobalEnv.color
  let pp = Color c p
  if b
    then nada
    else modify (\s -> s { pics = pp:pics s })

resetPics :: MonadLogo m => m ()
resetPics = modify (\s -> s { pics = [] })

tail' :: [a] -> [a]
tail' [] = []
tail' x = tail x

undoPic :: MonadLogo m => m ()
undoPic = modify (\s -> s { pics = tail' $ pics s })

setX :: MonadLogo m => Float -> m ()
setX n = modify (\s -> s { posx = n })

changeX :: MonadLogo m => (Float -> Float -> Float) -> Float -> m ()
changeX f n = getData posx >>= \x -> setX (f x n)

setY :: MonadLogo m => Float -> m ()
setY n = modify (\s -> s { posy = n })

changeY :: MonadLogo m => (Float -> Float -> Float) -> Float -> m ()
changeY f n = getData posy >>= \y -> setY (f y n)

setDir :: MonadLogo m => Float -> m ()
setDir n = modify (\s -> s { dirRad = grad2radian n, dirDeg = n })

changeDir :: MonadLogo m => (Float -> Float -> Float) -> Float -> m ()
changeDir f n = modify
  (\s -> s { dirRad = normalizeRad $ f (dirRad s) $ grad2radian n
           , dirDeg = normalizeDeg $ f (dirDeg s) n
           })

setColor :: MonadLogo m => Int -> m ()
setColor n = modify (\s -> s { GlobalEnv.color = num2color n })

setCustomColor :: MonadLogo m => Float -> Float -> Float -> m ()
setCustomColor r g b = modify (\s -> s { GlobalEnv.color = customColor r g b })

setScale :: MonadLogo m => Float -> m ()
setScale n = modify (\s -> s { escala = n })

setSizeT :: MonadLogo m => Float -> m ()
setSizeT n = modify (\s -> s { sizeT = n })

showT :: MonadLogo m => m ()
showT = modify (\s -> s { GlobalEnv.show = True })

hideT :: MonadLogo m => m ()
hideT = modify (\s -> s { GlobalEnv.show = False })

penUp :: MonadLogo m => m ()
penUp = modify (\s -> s { pen = True })

penDn :: MonadLogo m => m ()
penDn = modify (\s -> s { pen = False })

newVar :: MonadLogo m => String -> Float -> m ()
newVar str e = modify (\s -> s { vars = M.insert str e (vars s) })

delVar :: MonadLogo m => String -> m ()
delVar str = do
  varr <- getData vars
  if M.notMember str varr
    then failLogo
      ("Variable: " ++ str ++ ", no está definida, no se puede eliminar.")
    else modify (\s -> s { vars = M.delete str varr })

getVar :: MonadLogo m => String -> m Float
getVar str = do
  varr <- getData vars
  let mv = M.lookup str varr
  case mv of
    Nothing -> failLogo
      ("Variable: " ++ str ++ ", no está definida, no se puede acceder.")
    Just e  -> return e

newComm :: MonadLogo m => String -> Int -> [Comm] -> m ()
newComm str n c = do
  comm <- getData comms
  when (M.member str comm) $ printLogo $ "comando: " ++ str ++ ", redefinido."
  modify (\s -> s { comms = M.insert str (n, c) comm })

delComm :: MonadLogo m => String -> m ()
delComm str = do
  comm <- getData comms
  if M.notMember str comm
    then failLogo
      ("Comando: " ++ str ++ ", no está definido, no se puede eliminar.")
    else modify (\s -> s { comms = M.delete str comm })

getComm :: MonadLogo m => String -> m (Int, [Comm])
getComm str = do
  comm <- getData comms
  let mv = M.lookup str comm
  case mv of
    Nothing -> failLogo
      ("Comando: " ++ str ++ ", no está definido, no se puede acceder.")
    Just c  -> return c

setWait :: MonadLogo m => Float -> m ()
setWait f = modify (\s -> s { wait = True, acumWait = 0, limitWait = f })

getRandom :: MonadLogo m => Float -> m Float
getRandom n = do
  s <- get
  let g = random s
      (m, g') = randomR (0, n) g
  modify (\r -> r { random = g' })
  return m

catchErrors :: MonadLogo m => m a -> m (Maybe a)
catchErrors c =
  catchError (Just <$> c) (\e -> liftIO $ print e >> return Nothing)

type Logo = StateT Env (ExceptT String IO)

instance MonadLogo Logo

runLogo :: Env -> Logo a -> IO (Either String (a, Env))
runLogo e m = runExceptT $ runStateT m e
