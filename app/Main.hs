{-# LANGUAGE LambdaCase #-}

module Main where

import Common (Comm, Exp, FileType (..), Input (..), Output (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isSpace)
import Data.Maybe (isNothing)
import Eval (eval)
import GHC.Float.RealFracMethods (floorFloatInt)
import GlobalEnv (Env (..), defaultEnv, showComm, showVars)
import Graphics.Gloss (Display (..), Picture, pictures, scale, white)
import Graphics.Gloss.Export (GifLooping (LoopingForever), exportPictureToPNG, exportPicturesToGif)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Lib (comm2Bound, getTortu, parserComm)
import MonadLogo (MonadLogo, runLogo)
import SimpleGetOpt (ArgDescr (NoArg, ReqArg), OptDescr (Option), OptSpec (..), getOpts)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Exit (exitSuccess)
import System.FilePath.Windows (takeExtension)
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
      Left err -> putMVar (out e) (Error err) >> return m
      Right [] -> putMVar (out e) Ready >> return m
      Right ys -> evalStepComm e $ map (\c -> ([], comm2Bound [] c)) ys
    Just (ToFile ft xs) -> toFile ft xs e >> exitSuccess -- Quería poder seguir después de guardar pero da error
    Just (LoadFile xs) ->
      getFile xs >>= \zs -> case parserComm zs of
        Left err -> putMVar (out e) (Error $ err ++ " En el archivo: " ++ xs) >> return m
        Right [] -> putMVar (out e) Ready >> return m
        Right ys -> evalStepComm e $ map (\c -> ([], comm2Bound [] c)) ys
    Just ListV -> let v = showVars e in putMVar (out e) (Show v) >> putMVar (out e) Ready >> return m
    Just ListC -> let c = showComm e in putMVar (out e) (Show c) >> putMVar (out e) Ready >> return m
step _ _ (Just [], e) = putMVar (out e) Ready >> return (Nothing, e)
step _ _ (Just xs, e) = evalStepComm e xs

toFile :: FileType -> FilePath -> Env -> IO ()
toFile PNG f e =
  let size = toSave e
      esc = escala e
      picss = scale esc esc $ pictures $ reverse $ pics e
   in exportPictureToPNG size white f picss
toFile GIF f e =
  let size = toSave e
      esc = escala e
      pcs = reverse $ pics e
      n = length pcs
   in exportPicturesToGif 1 LoopingForever size white f (\x -> scale esc esc $ pictures $ take (floorFloatInt x) pcs) [0 .. (fromIntegral n)]

data Mode = Interactive | CompileFile deriving (Show)

data Argumentos = Argumentos
  { fullscreen :: Bool,
    width :: Int,
    height :: Int,
    refresh :: Int,
    mode :: Mode,
    files :: [FilePath]
  }

options :: OptSpec Argumentos
options =
  OptSpec
    { progDefaults = Argumentos False 300 300 60 Interactive [],
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
                _ -> Left "Valor inválido para 'refresh'",
          Option
            ['m']
            ["mode"]
            "Define el modo de funcionamiento del sistema, por defecto es interactivo."
            $ ReqArg "CHAR" $ \a s ->
              case a of
                'i' : _ -> Right s {mode = Interactive}
                'c' : _ -> Right s {mode = CompileFile}
                _ -> Left "Valor inválido para 'modo'"
        ],
      progParamDocs =
        [("FILES", "Los archivos que se quieran evaluar.")],
      progParams = \p s -> Right s {files = p : files s}
    }

main :: IO ()
main = do
  args <- getOpts options
  let d = case args of
        Argumentos True _ _ _ _ _ -> FullScreen
        Argumentos False w h _ _ _ -> makeWindow w h
  entrada <- newEmptyMVar
  salida <- newEmptyMVar
  g <- initStdGen
  case mode args of
    Interactive -> runProgramI d g entrada salida args
    CompileFile -> runProgramC d g entrada salida args

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
sendI input i o = liftIO (putMVar i input) >> waiter i o

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
    End -> return ()

hiloConsola :: MVar Input -> MVar Output -> IO ()
hiloConsola i o = runInputT defaultSettings (waiter i o)

runProgramI :: Display -> StdGen -> MVar Input -> MVar Output -> Argumentos -> IO ()
runProgramI d g i o args
  | null (files args) = noComm d g i o args
  | otherwise =
    compileFiles (files args) >>= \cms ->
      let cms' = map (comm2Bound []) cms
       in eval1st cms' g i o (width args, height args) >>= \case
            Left str -> print str
            Right (m, e) -> when (isNothing m) (putMVar o Ready) >> forkRun d m e i o (refresh args)

compileFiles :: [FilePath] -> IO [Comm]
compileFiles [] = return []
compileFiles (x : xs) =
  getFile x >>= \case
    "" -> compileFiles xs
    ys -> case parserComm ys of
      Left err -> putStrLn (err ++ " En el archivo: " ++ x ++ ".") >> return []
      Right zs -> compileFiles xs >>= return . (zs ++)

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

runProgramC :: Display -> StdGen -> MVar Input -> MVar Output -> Argumentos -> IO ()
runProgramC d g i o args = case files args of
  [] -> putStrLn "No se ingresó ningun archivo para el modo de Compilado."
  xs ->
    compileFiles xs >>= \case
      [] -> return ()
      ys ->
        forkIO (hiloConsola i o)
          >> let zs = map (comm2Bound []) ys
              in runLogo (defaultEnv g i o (width args, height args)) (evalFull zs []) >>= \case
                   Left str -> putMVar o $ Error str
                   Right (_, e) -> save2File e i o

save2File :: Env -> MVar Input -> MVar Output -> IO ()
save2File e i o = do
  putMVar o $ Show "Ingrese el nombre del archivo"
  putMVar o Ready
  s <- takeMVar i
  case s of
    Input ss ->
      let f = takeWhile (not . isSpace) $ dropWhile isSpace ss
          ext = takeExtension f
       in case ext of
            ".gif" -> toFile GIF f e
            _ -> toFile PNG f e
    _ -> save2File e i o

evalFull :: MonadLogo m => [Comm] -> [Exp] -> m ()
evalFull [] _ = return ()
evalFull (x : xs) e =
  eval e x >>= \m -> maybe (return ()) evalFull' m >> evalFull xs e

evalFull' :: MonadLogo m => [([Exp], Comm)] -> m ()
evalFull' [] = return ()
evalFull' ((e, c) : xs) = evalFull [c] e >> evalFull' xs