module Main where

import Common (Comm)
-- import Control.Monad
-- import Control.Monad.Catch (MonadMask)
-- import Control.Monad.IO.Class
import Eval (eval)
-- import LogoPar

-- import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import GlobalEnv
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Display (Controller (controllerSetRedraw))
import Graphics.Gloss.Interface.IO.Interact (Controller (controllerSetRedraw))
import Lib
import MonadLogo
import MonadLogo
  ( MonadLogo,
    getInput,
    printLogo,
    runLogo,
    runLogo',
  )
import System.Environment
import Text.Read

defaultHW :: Int
defaultHW = 300

makeWindow :: Int -> Int -> Display
makeWindow h w = InWindow "LOGO" (h, w) (0, 0)

defaultWindow :: Display
defaultWindow = makeWindow defaultHW defaultHW

getInputLine :: String -> IO String
getInputLine s = putStr s >> getLine

runProgram :: Display -> IO ()
runProgram d = animateIO d white (run defaultEnv) controllerSetRedraw

run :: Env -> Float -> IO Picture
-- run d = runLogo d (runInputT defaultSettings (repl)) >> return ()
run e f = do
  minput <- getInputLine inp
  case minput of
    "" -> run e f
    _ -> case parserComm minput of
      Nothing -> run e f
      Just cms -> do
        e' <- evalPrint e cms
        let x = posx e'
            y = posy e'
            d = dir e'
            b = GlobalEnv.show e'
            picc = pics e'
            piccc =
              if b
                then getTortu x y d : picc
                else picc
        return $ pictures piccc

evalPrint :: Env -> [Comm] -> IO Env
-- evalPrint e [] = return e
evalPrint e xs = do
  -- controllerSetRedraw
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

-- repl :: MonadLogo m => m ()
-- repl = do
--   minput <- getInput inp
--   case minput of
--     "" -> repl
--     _ -> case parserComm minput of
--       Nothing -> repl
--       Just cms -> (printLogo $ Prelude.show cms) >> evalinteractivo cms >> repl

-- repl :: (MonadLogo m, MonadMask m) => InputT m ()
-- repl = do
--   minput <- getInputLine inp
--   case minput of
--     Nothing -> return ()
--     Just "" -> repl
--     Just x -> do
--       case parserComm x of
--         Nothing -> repl
--         Just cms -> (liftIO $ putStrLn $ show cms) >> evalinteractivo cms >> repl

evalinteractivo :: MonadLogo m => [Comm] -> m ()
evalinteractivo = foldr ((>>) . eval []) (return ())
