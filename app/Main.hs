{-# LANGUAGE LambdaCase #-}

module Main where

import Common (Comm, Exp)
import Control.Concurrent (forkIO)
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bifunctor as B
import Data.Char (isSpace)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Eval (eval)
import GlobalEnv (Env (dir, pics, posx, posy, show), defaultEnv)
import Graphics.Gloss
  ( Display (..),
    Picture,
    pictures,
    scale,
    white,
  )
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Lib (comm2Bound, getTortu, parserComm)
import MonadLogo (runLogo')
import SimpleGetOpt
  ( ArgDescr (NoArg, ReqArg),
    OptDescr (Option),
    OptSpec (..),
    getOpts,
  )
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

makeWindow :: Int -> Int -> Display
makeWindow w h = InWindow "LOGO" (w, h) (0, 0)

type Model = (IORef String, Maybe [([Exp], Comm)], Env)

env2Pic :: Model -> IO Picture
env2Pic (_, _, e) =
  let x = posx e
      y = posy e
      d = dir e
      b = GlobalEnv.show e
      picc = pics e
      piccc =
        if b
          then reverse $ getTortu x y d : picc
          else picc
   in return (scale 5 5 $ pictures piccc)

evalStepComm :: IORef String -> Env -> [([Exp], Comm)] -> IO Model
evalStepComm _ _ [] = undefined -- No debería llegar a acá
evalStepComm ref e ((ex, x) : xs) =
  runLogo' e (eval ex x) >>= \case
    Left str -> putStrLn str >> return (ref, Nothing, e) -- Que hacer en caso de error?
    Right (mayComm, e') -> let retComm = maybe xs (++ xs) mayComm in return (ref, Just retComm, e')

step :: a -> b -> Model -> IO Model
step _ _ m@(ref, Nothing, e) = do
  input <- readIORef ref
  case input of
    "" -> return m
    ":q" -> exitSuccess -- Todo el programa termina
    _ ->
      atomicWriteIORef ref "" >> case parserComm input of
        Nothing -> print "no parse" >> return m
        Just [] -> return m
        Just xs -> evalStepComm ref e $ map (\c -> ([], c)) xs
step _ _ (ref, Just [], e) = return (ref, Nothing, e)
step _ _ (ref, Just xs, e) = evalStepComm ref e xs

data Argumentos = Argumentos
  { fullscreen :: Bool,
    width :: Int,
    height :: Int,
    refresh :: Int,
    files :: [FilePath]
  }
  deriving (Show)

options :: OptSpec Argumentos
options =
  OptSpec
    { progDefaults = Argumentos False 300 300 60 [],
      progOptions =
        [ Option
            ['f']
            ["fullscreen"]
            "Muestra la ventana gráfica en pantalla completa."
            $ NoArg $ \s -> Right s {fullscreen = True},
          Option
            ['h']
            ["height"]
            "Define la altura de la ventana gráfica si se muestra en ventana."
            $ ReqArg "NUM" $ \a s ->
              case readMaybe a of
                Just n | n > 0 -> Right s {height = n}
                _ -> Left "Valor inválido para 'height'",
          Option
            ['w']
            ["width"]
            "Define el ancho de la ventana gráfica si se muestra en ventana."
            $ ReqArg "NUM" $ \a s ->
              case readMaybe a of
                Just n | n > 0 -> Right s {width = n}
                _ -> Left "Valor inválido para 'width'",
          Option
            ['r']
            ["refresh"]
            "Define la velocidad de refresco del dibujo, por defecto es 60."
            $ ReqArg "NUM" $ \a s ->
              case readMaybe a of
                Just n | n > 0 -> Right s {refresh = n}
                _ -> Left "Valor inválido para 'refresh'"
        ],
      progParamDocs =
        [("FILES", "Los archivos que se quieran evaluar.")],
      progParams = \p s -> Right s {files = p : files s}
    }

main :: IO ()
main = do
  args <- getOpts options
  let d = case args of
        Argumentos True _ _ _ _ -> FullScreen
        Argumentos False w h _ _ -> makeWindow w h
  ref <- newIORef ""
  runProgram d (files args) ref (refresh args)

getFile :: FilePath -> IO String
getFile f =
  let fname = takeWhile (not . isSpace) f
   in catch
        (readFile fname)
        ( \e -> do
            let err = Prelude.show (e :: IOException)
            putStrLn ("No se pudo abrir el archivo: " ++ err)
            return ""
        )

inp :: String
inp = ">> "

consola :: IORef String -> InputT IO ()
consola ref = do
  input <- getInputLine inp
  case input of
    Nothing -> liftIO (atomicWriteIORef ref ":q")
    Just "" -> consola ref
    Just ":q" -> liftIO (atomicWriteIORef ref ":q") -- La diferencia es que no hace loop
    Just x -> liftIO (atomicWriteIORef ref x) >> consola ref

hiloConsola :: IORef String -> IO ()
hiloConsola ref = runInputT defaultSettings (consola ref)

runProgram :: Display -> [FilePath] -> IORef String -> Int -> IO ()
runProgram d fs ref r =
  mapM getFile fs >>= \s -> case parserComm $ concat s of
    Nothing -> print "Parse error en archivos" >> forkRun d Nothing defaultEnv ref r
    Just cms ->
      let cms' = map (comm2Bound []) cms
       in eval1st cms' >>= \case
            Left str -> print str
            Right (m, e) -> forkRun d m e ref r

forkRun :: Display -> Maybe [([Exp], Comm)] -> Env -> IORef String -> Int -> IO ()
forkRun d m e ref hz = forkIO (hiloConsola ref) >> simulateIO d white hz (ref, m, e) env2Pic step

eval1st :: [Comm] -> IO (Either String (Maybe [([Exp], Comm)], Env))
eval1st [] = return $ return (Nothing, defaultEnv)
eval1st (x : xs) =
  let ys = map (\c -> ([], c)) xs
   in runLogo' defaultEnv (eval [] x) >>= \zs -> return $ fmap (B.first (fmap (++ ys))) zs