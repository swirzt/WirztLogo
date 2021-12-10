module Main where

import Common (Comm)
import Control.Exception (IOException, catch)
import Data.Char
import Eval (eval)
import GlobalEnv
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate (ViewPort, simulateIO)
import Lib
import MonadLogo
  ( MonadLogo,
    runLogo,
    runLogo',
  )
import SimpleGetOpt
import System.Environment
-- import System.FilePath.Windows (replaceExtension)
import Text.Read (readMaybe)

makeWindow :: Int -> Int -> Display
makeWindow h w = InWindow "LOGO" (h, w) (0, 0)

env2Pic :: (a, b, Env) -> IO Picture
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

getComm :: Env -> IO (Float, Maybe [([Exp], Comm)], Env)
getComm = do
  minput <- getLine
  case minput of
    "" -> return (0, e)
    _ -> case parserComm minput of
      Nothing -> print "no parse" >> return (0, Nothing, e)
      Just cms -> 

step :: a -> Float -> (Float, Maybe [([Exp], Comm)], Env) -> IO (Float, Maybe [([Exp], Comm)], Env)
step _ f (fe, Nothing, e)
  | fe < 2 = return (fe + f, Nothing, e)
  | otherwise = getComm e
step _ _ (fe, Just)

evalPrint :: Env -> [Comm] -> IO Env
evalPrint e xs = do
  r <- runLogo' e (evalinteractivo xs)
  case r of
    Left s -> putStrLn s >> return defaultEnv
    Right (_, e') -> return e'

data Argumentos = Argumentos
  { fullscreen :: Bool,
    width :: Int,
    height :: Int,
    files :: [FilePath]
  }
  deriving (Show)

options :: OptSpec Argumentos
options =
  OptSpec
    { progDefaults = Argumentos False 300 300 [],
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
                _ -> Left "Valor inválido para 'width'"
        ],
      progParamDocs =
        [("FILES", "Los archivos que se quieran evaluar.")],
      progParams = \p s -> Right s {files = p : files s}
    }

main :: IO ()
main = do
  args <- getOpts options
  let d = case args of
        Argumentos True _ _ _ -> FullScreen
        Argumentos False w h _ -> makeWindow h w
  runProgram d (files args)

getFile :: FilePath -> IO String
getFile f =
  let fname = takeWhile (not . isSpace) f
   in catch
        (readFile fname)
        ( \e -> do
            let error = Prelude.show (e :: IOException)
            putStrLn ("No se pudo abrir el archivo: " ++ error)
            return ""
        )

runProgram :: Display -> [FilePath] -> IO ()
runProgram d fs =
  mapM getFile fs >>= \s -> case parserComm $ concat s of
    Nothing -> print "Parse error en archivos"
    Just cms ->
      runLogo (evalinteractivo cms) >>= \x -> case x of
        Left str -> print str
        Right (_, e) -> simulateIO d white 60 (0, e) env2Pic step

inp :: String
inp = ">> "

evalinteractivo :: MonadLogo m => [Comm] -> m ()
evalinteractivo = foldr ((>>) . eval []) (return ())
