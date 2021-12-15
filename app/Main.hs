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
import Eval (EvalRet, eval)
import GHC.Float.RealFracMethods (floorFloatInt)
import GlobalEnv (Env (..), defaultEnv, showComm, showVars)
import Graphics.Gloss (Display (..), Picture, pictures, scale, white)
import Graphics.Gloss.Export (GifLooping (LoopingForever), exportPictureToPNG, exportPicturesToGif)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Lib (comm2Bound, getTortu, parserComm)
import MonadLogo (MonadLogo, runLogo)
import SimpleGetOpt (ArgDescr (NoArg, ReqArg), OptDescr (Option), OptSpec (..), getOpts)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
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
  entrada <- newEmptyMVar
  salida <- newEmptyMVar
  g <- initStdGen
  let d = case args of
        Argumentos True _ _ _ _ _ -> FullScreen
        Argumentos False w h _ _ _ -> makeWindow w h
      e = defaultEnv g entrada salida (width args, height args)
  forkIO $ hiloConsola entrada salida
  case mode args of
    Interactive -> runProgramI e d args

-- CompileFile -> runProgramC e args

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

-- Recibe un Input, lo envía por el canal de Input y va a esperar
sendI :: Input -> MVar Input -> MVar Output -> InputT IO ()
sendI input i o = liftIO (putMVar i input) >> waiter i o

-- Recibe un input, lo envía y termina la ejecución de consola
exit :: MVar Input -> Input -> InputT IO ()
exit i input = liftIO (putMVar i input)

-- Lee un input de consola y lo envía por la variable Input
-- El input puede ser una instrucción precedida por ':'
{- Instrucciones:
   - :q - Termina la ejecución
   - :v - Lista los valores guardados de variables
   - :c - Lista las definiciones guardadas de comandos
   - :l archivo.logo - Carga el archivo con comandos
   - :sg archivo.gif - Guarda el dibujo actual en un GIF animado
   - :sp archivo.png - Guarda el dibujo actual en una imagen PNG
-}
-- De lo contrario envía el input como fué ingresado
consola :: MVar Input -> MVar Output -> InputT IO ()
consola i o = do
  input <- getInputLine ">> "
  case input of
    Nothing -> exit i Exit
    Just "" -> consola i o
    Just ":q" -> exit i Exit
    Just ":v" -> sendI ListV i o
    Just ":c" -> sendI ListC i o
    Just (':' : 'l' : xs) -> case makeFileName xs of
      Nothing -> outputStrLn "No se otorgó un nombre de archivo" >> consola i o
      Just ys -> sendI (LoadFile ys) i o
    Just (':' : 's' : 'g' : xs) -> case makeFileName xs of
      Nothing -> outputStrLn "No se otorgó un nombre de archivo" >> consola i o
      Just ys -> exit i (ToFile GIF ys)
    Just (':' : 's' : 'p' : xs) -> case makeFileName xs of
      Nothing -> outputStrLn "No se otorgó un nombre de archivo" >> consola i o
      Just ys -> exit i (ToFile PNG ys)
    Just x -> sendI (Input x) i o

makeFileName :: String -> Maybe FilePath
makeFileName str = case dropWhile isSpace str of
  [] -> Nothing
  f -> Just f

-- Funciona igual que conssola pero no revisa instrucciones
-- Las envía tal cual ingresaron
inputExp :: MVar Input -> MVar Output -> InputT IO ()
inputExp i o = do
  input <- getInputLine ">> "
  case input of
    Nothing -> liftIO (putMVar i Exit)
    Just "" -> inputExp i o
    Just x -> liftIO (putMVar i (Input x)) >> waiter i o

-- Función de espera mientras se evaluan comandos
{- Puede recibir los siguientes Outputs:
   - Ready -> Se terminó de evaluar y se espera entrada de consola
   - GetExp -> Se espera entrada sin parseo de instrucciones ':'
   - Error str -> La evaluación dió error y se espera entrada de consola
   - Show str -> Se imprime algo durante la evaluación, se sigue evaluando
-}
waiter :: MVar Input -> MVar Output -> InputT IO ()
waiter i o = do
  output <- liftIO $ takeMVar o
  case output of
    Ready -> consola i o
    GetExp -> inputExp i o
    Error xs -> outputStrLn xs >> consola i o
    Show xs -> outputStrLn xs >> waiter i o

-- Función de lift IO para la consola
hiloConsola :: MVar Input -> MVar Output -> IO ()
hiloConsola i o = runInputT defaultSettings (waiter i o)

-- Correr programa de forma Interactiva
runProgramI :: Env -> Display -> Argumentos -> IO ()
runProgramI e d args =
  case files args of
    [] -> simulateIO d white (refresh args) (Just [], e) env2Pic step
    fs ->
      compileFiles (out e) fs >>= \cms ->
        let cms' = map (\c -> ([], comm2Bound [] c)) cms
         in simulateIO d white (refresh args) (Just cms', e) env2Pic step

-- Recibe una lista de archivos, los abre y parsea
-- Si encuentra un error lo muestra en consola y continúa compilando
-- Devuelve la lista de todos los comandos concatenada
compileFiles :: MVar Output -> [FilePath] -> IO [Comm]
compileFiles _ [] = return []
compileFiles o (x : xs) =
  getFile x >>= \case
    "" -> putMVar o (Show $ "Archivo vacío: " ++ x) >> compileFiles o xs
    ys -> case parserComm ys of
      Left err -> putMVar o (Show (err ++ " En el archivo: " ++ x ++ ".")) >> compileFiles o xs
      Right zs -> compileFiles o xs >>= return . (zs ++)

runProgramC :: Env -> Argumentos -> IO ()
runProgramC e args = case files args of
  [] -> putMVar (out e) $ Show "No se ingresó ningun archivo para el modo de Compilado."
  [x] -> putMVar (out e) $ Show "Se debe ingresar al menos 2 archivos, el último es el nombre del archivo a guardar."
  x : xs ->
    compileFiles (out e) xs >>= \case
      [] -> return ()
      ys ->
        let zs = map (comm2Bound []) ys
         in runLogo e (evalFull zs []) >>= \case
              Left str -> putMVar (out e) $ Error str
              Right (_, e) -> save2File e x

save2File :: Env -> FilePath -> IO ()
save2File e f = case takeExtension f of
  ".gif" -> toFile GIF f e
  _ -> toFile PNG f e

evalFull :: MonadLogo m => [Comm] -> [Exp] -> m ()
evalFull [] _ = return ()
evalFull (x : xs) e =
  eval e x >>= \m -> maybe (return ()) evalFull' m >> evalFull xs e

evalFull' :: MonadLogo m => [([Exp], Comm)] -> m ()
evalFull' [] = return ()
evalFull' ((e, c) : xs) = evalFull [c] e >> evalFull' xs