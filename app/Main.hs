{-# LANGUAGE LambdaCase #-}

module Main where

-- Imports de librerias
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar
                                        , tryTakeMVar)
import           Control.Exception (IOException, catch)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Char (isSpace)
import           GHC.Float.RealFracMethods (float2Int)
import           Graphics.Gloss (Display(..), Picture, pictures, scale, white)
import           Graphics.Gloss.Export (GifLooping(LoopingForever)
                                      , exportPictureToPNG, exportPicturesToGif)
import           Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import           SimpleGetOpt (ArgDescr(NoArg, ReqArg), OptDescr(Option)
                             , OptSpec(..), getOpts)
import           System.Console.Haskeline (InputT, defaultSettings, getInputLine
                                         , outputStrLn, runInputT)
import           System.Exit (exitSuccess)
import           System.FilePath (takeExtension, replaceExtension)
import           System.Random (initStdGen)
import           Text.Read (readMaybe)
-- Imports locales
import           Common (Comm, Exp, FileType(..), Input(..), Output(..))
import           Eval (eval, EvalRet)
import           GlobalEnv (Env(..), defaultEnv, showComm, showVars)
import           Lib (comm2Bound, getTortu, parserComm)
import           MonadLogo (MonadLogo, runLogo)

makeWindow :: Int -> Int -> Display
makeWindow w h = InWindow "LOGO" (w, h) (0, 0)

type Model = (Maybe EvalRet, Env)

-- Crea una Picture a partir del entorno actual
env2Pic :: Model -> IO Picture
env2Pic (_, e) =
  let x = posx e
      y = posy e
      d = dirDeg e
      b = GlobalEnv.show e
      picc = pics e
      esc = escala e
      piccc = if b
              then reverse $ getTortu x y d:picc
              else picc
  in return (scale esc esc $ pictures piccc)

-- Ejecuta un paso del programa
evalStepComm :: Env -> EvalRet -> IO Model
evalStepComm _ [] = undefined -- No deber??a llegar a esto
evalStepComm e ((ex, x):xs) = runLogo e (eval ex x)
  >>= \case
    Left str -> putMVar (out e) (Error str) >> return (Nothing, e) -- Que hacer en caso de error?
    Right (listComm, e') -> let retComm = listComm ++ xs
                            in return (Just retComm, e')

inputHandler :: Model -> Input -> IO Model
inputHandler _ Exit = exitSuccess -- Todo el programa termina
inputHandler m@(_, e) (Input xs) = case parserComm xs of
  Left err -> putMVar (out e) (Error err) >> return m -- El parseo fall??
  Right [] -> putMVar (out e) Ready >> return m -- El parseo di?? vac??o
  Right ys -> evalStepComm e $ map (\c -> ([], comm2Bound [] c)) ys -- El parseo funcion??
inputHandler (_, e) (ToFile ft xs) = toFile ft xs e >> exitSuccess -- Quer??a poder seguir despu??s de guardar pero da error
inputHandler m@(_, e) (LoadFile xs) = getFile xs
  >>= \zs -> case parserComm zs of
    Left err -> putMVar (out e) (Error $ err ++ " En el archivo: " ++ xs)
      >> return m
    Right [] -> putMVar (out e) Ready >> return m
    Right ys -> evalStepComm e $ map (\c -> ([], comm2Bound [] c)) ys
inputHandler m@(_, e) ListV =
  let v = showVars e
  in putMVar (out e) (Show v) >> putMVar (out e) Ready >> return m
inputHandler m@(_, e) ListC =
  let c = showComm e
  in putMVar (out e) (Show c) >> putMVar (out e) Ready >> return m

-- Avanza el modelo un paso
step :: a -> Float -> Model -> IO Model
step _ f (n, e)
  | wait e = let newAcum = acumWait e + (f * 1000)
                 e' = if newAcum >= limitWait e
                      then e { wait = False }
                      else e { acumWait = newAcum }
             in return (n, e')
step _ _ m@(Nothing, e) = do
  input <- tryTakeMVar $ inp e -- Intenta leer de la MVar
  case input of
    Nothing   -> return m -- No hay cosas nuevas
    Just Exit -> exitSuccess -- Todo el programa termina
    Just i    -> inputHandler m i
step _ _ (Just [], e) = putMVar (out e) Ready >> return (Nothing, e) -- Se termin?? la ejecuci??n actual
step _ _ (Just xs, e) = evalStepComm e xs -- Se puede seguir ejecutando

-- Recibe el tipo de archivo y donde guardarlo, guarda la imagen generada por el entorno
toFile :: FileType -> FilePath -> Env -> IO ()
toFile PNG f e = let size = toSave e
                     esc = escala e
                     picss = scale esc esc $ pictures $ reverse $ pics e
                 in exportPictureToPNG size white f picss
toFile GIF f e =
  let size = toSave e
      esc = escala e
      pcs = reverse $ pics e
      n = length pcs
  in exportPicturesToGif
       1
       LoopingForever
       size
       white
       f
       (\x -> scale esc esc $ pictures $ take (float2Int x) pcs)
       [0 .. (fromIntegral n)]

-- Modos de funcionamiento del programa
data Mode = Interactive
          | CompileFile
  deriving (Show)

-- Estructura de los argumentos de consola
data Argumentos =
  Argumentos { fullscreen :: Bool
             , width :: Int
             , height :: Int
             , refresh :: Int
             , mode :: Mode
             , files :: [FilePath]
             }

-- Parseo de argumentos
options :: OptSpec Argumentos
options =
  OptSpec { progDefaults = Argumentos False 300 300 60 Interactive []
          , progOptions =
              [ Option
                  ['f']
                  ["fullscreen"]
                  "Muestra la ventana gr??fica en pantalla completa."
                $ NoArg
                $ \s -> Right s { fullscreen = True }
              , Option
                  ['h']
                  ["height"]
                  "Define la altura de la ventana gr??fica si se muestra en ventana."
                $ ReqArg "NUM"
                $ \a s -> case readMaybe a of
                  Just n
                    | n > 0 -> Right s { height = n }
                  _      -> Left "Valor inv??lido para 'height'"
              , Option
                  ['w']
                  ["width"]
                  "Define el ancho de la ventana gr??fica si se muestra en ventana."
                $ ReqArg "NUM"
                $ \a s -> case readMaybe a of
                  Just n
                    | n > 0 -> Right s { width = n }
                  _      -> Left "Valor inv??lido para 'width'"
              , Option
                  ['r']
                  ["refresh"]
                  "Define la velocidad de refresco del dibujo, por defecto es 60."
                $ ReqArg "NUM"
                $ \a s -> case readMaybe a of
                  Just n
                    | n > 0 -> Right s { refresh = n }
                  _      -> Left "Valor inv??lido para 'refresh'"
              , Option
                  ['m']
                  ["mode"]
                  "Define el modo de funcionamiento del sistema, por defecto es interactivo."
                $ ReqArg "CHAR"
                $ \a s -> case a of
                  'i':_ -> Right s { mode = Interactive }
                  'c':_ -> Right s { mode = CompileFile }
                  _     -> Left "Valor inv??lido para 'modo'"]
          , progParamDocs = [("FILES", "Los archivos que se quieran evaluar.")]
          , progParams = \p s -> Right s { files = p:files s }
          }

main :: IO ()
main = do
  args <- getOpts options
  entrada <- newEmptyMVar
  salida <- newEmptyMVar
  g <- initStdGen
  let d = case args of
        Argumentos True _ _ _ _ _  -> FullScreen
        Argumentos False w h _ _ _ -> makeWindow w h
      e = defaultEnv g entrada salida (width args, height args)
  _ <- forkIO $ hiloConsola entrada salida
  case mode args of
    Interactive -> runProgramI e d args
    CompileFile -> runProgramC e args

-- Recibe un archivo e intenta abrirlo
getFile :: FilePath -> IO String
getFile f = let fname = takeWhile (not . isSpace) f
            in catch
                 (readFile fname)
                 (\e -> do
                    let err = Prelude.show (e :: IOException)
                    putStrLn ("No se pudo abrir el archivo: " ++ err)
                    return "")

-- Recibe un Input, lo env??a por el canal de Input y va a esperar
sendI :: Input -> MVar Input -> MVar Output -> InputT IO ()
sendI input i o = liftIO (putMVar i input) >> waiter i o

-- Recibe un input, lo env??a y termina la ejecuci??n de consola
exit :: MVar Input -> Input -> InputT IO ()
exit i input = liftIO (putMVar i input)

-- Lee un input de consola y lo env??a por la variable Input
-- El input puede ser una instrucci??n precedida por ':'
{- Instrucciones:
   - :? - Muestra el men?? de ayuda
   - :q - Termina la ejecuci??n
   - :v - Lista los valores guardados de variables
   - :c - Lista las definiciones guardadas de comandos
   - :l archivo.logo - Carga el archivo con comandos
   - :sg archivo.gif - Guarda el dibujo actual en un GIF animado
   - :sp archivo.png - Guarda el dibujo actual en una imagen PNG
-}
-- De lo contrario env??a el input como fu?? ingresado
consola :: MVar Input -> MVar Output -> InputT IO ()
consola i o = do
  input <- getInputLine ">> "
  case input of
    Nothing -> exit i Exit
    Just "" -> consola i o
    Just ":q" -> exit i Exit
    Just ":v" -> sendI ListV i o
    Just ":c" -> sendI ListC i o
    Just ":?" -> outputStrLn mensajeAyuda >> consola i o
    Just (':':'l':xs) -> case makeFileName xs of
      Nothing
        -> outputStrLn "No se otorg?? un nombre de archivo" >> consola i o
      Just ys -> sendI (LoadFile ys) i o
    Just (':':'s':'g':xs) -> case makeFileName xs of
      Nothing
        -> outputStrLn "No se otorg?? un nombre de archivo" >> consola i o
      Just ys -> exit i (ToFile GIF ys)
    Just (':':'s':'p':xs) -> case makeFileName xs of
      Nothing
        -> outputStrLn "No se otorg?? un nombre de archivo" >> consola i o
      Just ys -> exit i (ToFile PNG ys)
    Just x -> sendI (Input x) i o

mensajeAyuda :: String
mensajeAyuda = "Instrucciones:\n"
  ++ "- :? - Muestra este men?? de ayuda\n"
  ++ "- :q - Termina la ejecuci??n\n"
  ++ "- :v - Lista los valores guardados de variables\n"
  ++ "- :c - Lista las definiciones guardadas de comandos\n"
  ++ "- :l archivo.logo - Carga el archivo con comandos\n"
  ++ "- :sg archivo.gif - Guarda el dibujo actual en un GIF animado\n"
  ++ "- :sp archivo.png - Guarda el dibujo actual en una imagen PNG"

makeFileName :: String -> Maybe FilePath
makeFileName str = case dropWhile isSpace str of
  [] -> Nothing
  f  -> Just f

-- Funciona igual que conssola pero no revisa instrucciones
-- Las env??a tal cual ingresaron
inputExp :: MVar Input -> MVar Output -> InputT IO ()
inputExp i o = do
  input <- getInputLine ">> "
  case input of
    Nothing -> liftIO (putMVar i Exit)
    Just "" -> inputExp i o
    Just x  -> liftIO (putMVar i (Input x)) >> waiter i o

-- Funci??n de espera mientras se evaluan comandos
{- Puede recibir los siguientes Outputs:
   - Ready -> Se termin?? de evaluar y se espera entrada de consola
   - GetExp -> Se espera entrada sin parseo de instrucciones ':'
   - Error str -> La evaluaci??n di?? error y se espera entrada de consola
   - Show str -> Se imprime algo durante la evaluaci??n, se sigue evaluando
-}
waiter :: MVar Input -> MVar Output -> InputT IO ()
waiter i o = do
  output <- liftIO $ takeMVar o
  case output of
    Ready    -> consola i o
    GetExp   -> inputExp i o
    Error xs -> outputStrLn xs >> consola i o
    Show xs  -> outputStrLn xs >> waiter i o

-- Funci??n de lift IO para la consola
hiloConsola :: MVar Input -> MVar Output -> IO ()
hiloConsola i o = runInputT defaultSettings (waiter i o)

-- Correr programa de forma Interactiva
runProgramI :: Env -> Display -> Argumentos -> IO ()
runProgramI e d args = case files args of
  [] -> simulateIO d white (refresh args) (Just [], e) env2Pic step
  fs -> compileFiles (out e) fs
    >>= \cms
    -> let cms' = map (\c -> ([], comm2Bound [] c)) cms
       in simulateIO d white (refresh args) (Just cms', e) env2Pic step

-- Recibe una lista de archivos, los abre y parsea
-- Si encuentra un error lo muestra en consola y contin??a compilando
-- Devuelve la lista de todos los comandos concatenada
compileFiles :: MVar Output -> [FilePath] -> IO [Comm]
compileFiles _ [] = return []
compileFiles o (x:xs) = getFile x
  >>= \case
    "" -> putMVar o (Show $ "Archivo vac??o: " ++ x) >> compileFiles o xs
    ys -> case parserComm ys of
      Left err -> putMVar o (Show (err ++ " En el archivo: " ++ x ++ "."))
        >> compileFiles o xs
      Right zs -> compileFiles o xs >>= return . (zs ++)

-- Correr programa en modos de compilado
runProgramC :: Env -> Argumentos -> IO ()
runProgramC e args = case files args of
  []   -> putMVar (out e)
    $ Show "No se ingres?? ningun archivo para el modo de Compilado."
  [_]  -> putMVar (out e)
    $ Show
      "Se debe ingresar al menos 2 archivos, el ??ltimo es el nombre del archivo a guardar."
  x:xs -> compileFiles (out e) xs
    >>= \case
      [] -> return ()
      ys -> let zs = map (comm2Bound []) ys
            in runLogo e (evalFull zs [])
               >>= \case
                 Left str      -> putMVar (out e) $ Error str
                 Right (_, e') -> save2File e' x

-- Recibe un estado y un archivo, llama a la funci??n toFile con la extensi??n apropiada
save2File :: Env -> FilePath -> IO ()
save2File e f = case takeExtension f of
  ".gif" -> toFile GIF f e
  ".png" -> toFile PNG f e
  _      -> toFile PNG (replaceExtension f ".png") e

-- Corre una lista de Comandos con un ??nico entorno hasta evaluar todo
evalFull :: MonadLogo m => [Comm] -> [Exp] -> m ()
evalFull [] _ = return ()
evalFull (x:xs) e = eval e x >>= evalFull' >> evalFull xs e

-- Corre una lista de Comandos y sus entornos hasta evaluar todo
evalFull' :: MonadLogo m => EvalRet -> m ()
evalFull' [] = return ()
evalFull' ((e, c):xs) = evalFull [c] e >> evalFull' xs
