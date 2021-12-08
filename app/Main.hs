module Main where

import Common (Comm)
-- import Control.Monad
-- import Control.Monad.Catch (MonadMask)
-- import Control.Monad.IO.Class

-- import LogoPar

import Control.Monad
import Eval (eval)
import GlobalEnv
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Display (Controller (controllerSetRedraw))
import Graphics.Gloss.Interface.IO.Interact (Controller (controllerSetRedraw))
import Graphics.Gloss.Interface.IO.Simulate (ViewPort, simulateIO)
import Lib
import MonadLogo
  ( MonadLogo,
    getInput,
    printLogo,
    runLogo,
    runLogo',
  )
import System.Console.Haskeline
import System.Environment
import Text.Read (readMaybe)

defaultHW :: Int
defaultHW = 300

makeWindow :: Int -> Int -> Display
makeWindow h w = InWindow "LOGO" (h, w) (0, 0)

defaultWindow :: Display
defaultWindow = makeWindow defaultHW defaultHW

runProgram :: Display -> IO ()
runProgram d = simulateIO d white 1000 defaultEnv env2Pic step

env2Pic :: Env -> IO Picture
env2Pic e =
  let x = posx e
      y = posy e
      d = dir e
      b = GlobalEnv.show e
      picc = pics e
      piccc =
        if b
          then getTortu x y d : picc
          else picc
   in return $ pictures piccc

step :: ViewPort -> Float -> Env -> IO Env
step v f e = do
  minput <- runInputT defaultSettings getInp
  case minput of
    "" -> step v f e
    _ -> case parserComm minput of
      Nothing -> step v f e
      Just cms -> evalPrint e cms
  where
    getInp :: InputT IO String
    getInp = getInputLine inp >>= maybe (return "") return

evalPrint :: Env -> [Comm] -> IO Env
evalPrint e xs = do
  r <- runLogo' e (evalinteractivo xs)
  case r of
    Left s -> putStrLn s >> return defaultEnv
    Right (_, e') -> return e'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runProgram defaultWindow
    (h : w : _) -> case readMaybe h >>= \hi -> readMaybe w >>= \wi -> return $ makeWindow hi wi of
      Just d -> runProgram d
      Nothing -> do
        putStrLn "Uno o ambos argumentos son inválidos\nUtilizando las medidas default"
        runProgram defaultWindow
    _ -> putStrLn "Numero incorrecto de argumentos"

inp :: String
inp = ">> "

evalinteractivo :: MonadLogo m => [Comm] -> m ()
evalinteractivo = foldr ((>>) . eval []) (return ())
