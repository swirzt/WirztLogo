{-# LANGUAGE LambdaCase #-}

module Main where

import Common (Comm, Exp, FileType (..), Input (..), Output (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bifunctor as B
import Data.Char (isSpace)
import Data.Maybe (isNothing)
import Eval (eval)
import GHC.Float.RealFracMethods (floorFloatInt)
import GlobalEnv (Env (..), defaultEnv, showComm, showVars)
import Graphics.Gloss (Display (..), Picture, pictures, scale, white)
import Graphics.Gloss.Export (GifLooping (LoopingForever), exportPictureToPNG, exportPicturesToGif)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Lib (comm2Bound, getTortu, parserComm)
import MonadLogo (runLogo)
import SimpleGetOpt (ArgDescr (NoArg, ReqArg), OptDescr (Option), OptSpec (..), getOpts)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Exit (exitSuccess)
import System.Random (StdGen, initStdGen)
import Text.Read (readMaybe)

makeWindow :: Int -> Int -> Display
makeWindow w h = InWindow "LOGO" (w, h) (0, 0)

type Model = (Maybe [([Exp], Comm)], Env)

env2Pic :: Model -> IO Picture
env2Pic (_, e) =
  let x = posx e
      y = posy e
      d = dir e
      b = GlobalEnv.show e
      picc = pics e
      esc = escala e
      piccc =
        if b
          then reverse $ getTortu x y d : picc
          else picc
   in return (scale esc esc $ pictures piccc)

evalStepComm :: Env -> [([Exp], Comm)] -> IO Model
evalStepComm _ [] = undefined -- No debería llegar a esto
evalStepComm e ((ex, x) : xs) =
  runLogo e (eval ex x) >>= \case
    Left str -> putMVar (out e) (Error str) >> return (Nothing, e) -- Que hacer en caso de error?
    Right (mayComm, e') -> let retComm = maybe xs (++ xs) mayComm in return (Just retComm, e')

step :: a -> b -> Model -> IO Model
step _ _ m@(Nothing, e) = do
  input <- tryTakeMVar $ inp e
  case input of
    Nothing -> return m
    Just Exit -> exitSuccess -- Todo el programa termina
    Just (Input xs) -> case parserComm xs of
      Nothing -> putMVar (out e) (Error "Parse error") >> return m
      Just [] -> putMVar (out e) Ready >> return m
      Just ys -> evalStepComm e $ map (\c -> ([], comm2Bound [] c)) ys
    Just (ToFile ft xs) -> toFile ft xs e >> exitSuccess -- Quería poder seguir después de guardar pero da error
    Just (LoadFile xs) ->
      getFile xs >>= \zs -> case parserComm zs of
        Nothing -> putMVar (out e) (Error $ "Parse error en archivo: " ++ xs) >> return m
        Just [] -> putMVar (out e) Ready >> return m
        Just ys -> evalStepComm e $ map (\c -> ([], comm2Bound [] c)) ys
    Just ListV -> let v = showVars e in putMVar (out e) (Show v) >> putMVar (out e) Ready >> return m
    Just ListC -> let c = showComm e in putMVar (out e) (Show c) >> putMVar (out e) Ready >> return m
step _ _ (Just [], e) = putMVar (out e) Ready >> return (Nothing, e)
step _ _ (Just xs, e) = evalStepComm e xs

toFile :: FileType -> FilePath -> Env -> IO ()
toFile PNG f e =
  let size = toSave e
      picss = pictures $ reverse $ pics e
   in exportPictureToPNG size white f picss
toFile GIF f e =
  let size = toSave e
      esc = escala e
      pcs = reverse $ pics e
      n = length pcs
   in exportPicturesToGif 1 LoopingForever size white f (\x -> scale esc esc $ pictures $ take (floorFloatInt x) pcs) [0 .. (fromIntegral n)]

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
  entrada <- newEmptyMVar
  salida <- newEmptyMVar
  g <- initStdGen
  runProgram d g entrada salida args

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

consoleSym :: String
consoleSym = ">> "

sendI :: Input -> MVar Input -> MVar Output -> InputT IO ()
sendI inp i o = liftIO (putMVar i inp) >> waiter i o

consola :: MVar Input -> MVar Output -> InputT IO ()
consola i o = do
  input <- getInputLine consoleSym
  case input of
    Nothing -> liftIO (putMVar i Exit)
    Just "" -> consola i o
    Just ":q" -> liftIO (putMVar i Exit)
    Just ":v" -> sendI ListV i o
    Just ":c" -> sendI ListC i o
    Just (':' : 'l' : xs) -> let ys = dropWhile isSpace xs in sendI (LoadFile ys) i o
    Just (':' : 's' : 'g' : xs) -> let ys = dropWhile isSpace xs in liftIO (putMVar i (ToFile GIF ys))
    Just (':' : 's' : 'p' : xs) -> let ys = dropWhile isSpace xs in liftIO (putMVar i (ToFile PNG ys))
    Just x -> sendI (Input x) i o

inputExp :: MVar Input -> MVar Output -> InputT IO ()
inputExp i o = do
  input <- getInputLine consoleSym
  case input of
    Nothing -> liftIO (putMVar i Exit)
    Just "" -> inputExp i o
    Just x -> liftIO (putMVar i (Input x)) >> waiter i o

waiter :: MVar Input -> MVar Output -> InputT IO ()
waiter i o = do
  output <- liftIO $ takeMVar o
  case output of
    Ready -> consola i o
    GetExp -> inputExp i o
    Error xs -> liftIO (putStrLn xs) >> consola i o
    Show xs -> liftIO (putStrLn xs) >> waiter i o

hiloConsola :: MVar Input -> MVar Output -> IO ()
hiloConsola i o = runInputT defaultSettings (waiter i o)

runProgram :: Display -> StdGen -> MVar Input -> MVar Output -> Argumentos -> IO ()
runProgram d g i o args
  | null (files args) = noComm d g i o args
  | otherwise =
    mapM getFile (files args) >>= \s -> case parserComm $ concat s of
      Nothing -> putStrLn "Parse error en archivos" >> noComm d g i o args
      Just cms ->
        let cms' = map (comm2Bound []) cms
         in eval1st cms' g i o (width args, height args) >>= \case
              Left str -> print str
              Right (m, e) -> when (isNothing m) (putMVar o Ready) >> forkRun d m e i o (refresh args)

noComm :: Display -> StdGen -> MVar Input -> MVar Output -> Argumentos -> IO ()
noComm d g i o args = putMVar o Ready >> forkRun d Nothing (defaultEnv g i o (width args, height args)) i o (refresh args)

forkRun :: Display -> Maybe [([Exp], Comm)] -> Env -> MVar Input -> MVar Output -> Int -> IO ()
forkRun d m e i o hz = forkIO (hiloConsola i o) >> simulateIO d white hz (m, e) env2Pic step

eval1st :: [Comm] -> StdGen -> MVar Input -> MVar Output -> (Int, Int) -> IO (Either String (Maybe [([Exp], Comm)], Env))
eval1st [] g i o s = return $ return (Nothing, defaultEnv g i o s)
eval1st (x : xs) g i o s =
  let ys = map (\c -> ([], c)) xs
   in runLogo (defaultEnv g i o s) (eval [] x) >>= \zs -> case zs of
        Left _ -> return zs
        Right (m, e) -> return $ Right (Just $ maybe ys (++ ys) m, e)