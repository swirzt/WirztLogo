module GlobalEnv (Env (..), defaultEnv, showVars, showComm) where

import Common (Comm, Exp, Input, Output, dropEnd, showCommList)
import Control.Concurrent.MVar (MVar)
import Data.Map (Map, empty, toList)
import Graphics.Gloss (Color, Picture, black)
import System.Random (StdGen)

data Env = Env
  { posx :: Float, -- Representa la posición x de la tortuga
    posy :: Float, -- Representa la posición y de la tortuga
    dir :: Float, -- Representa la dirección a donde mira la tortuga, representa los grados entre 0 y 2pi
    show :: Bool, -- Si es True, la tortuga se imprime en pantalla
    pen :: Bool, -- Si es True, los movimientos de la tortuga no dibujan
    color :: Color, -- Color del lápiz
    pics :: [Picture], -- Imagenes ya calculadas en el entorno
    vars :: Map String Float, -- Almacena las definiciones de Variables en el entorno
    comms :: Map String (Int, [Comm]), -- Almacena las definiciones de Comandos en el entorno
    escala :: Float, -- Cuanto se debe escalar la imagen
    sizeT :: Float,
    random :: StdGen, -- Semilla para generación de numeros random
    inp :: MVar Input, -- Variable de comunicación de entrada
    out :: MVar Output, -- Variable de comunicación de salida
    toSave :: (Int, Int) -- Tamaño en píxeles de la pantalla y como se guardará en el archivo
  }

defaultEnv :: StdGen -> MVar Input -> MVar Output -> (Int, Int) -> Env
defaultEnv = Env 0 0 0 True False black [] empty empty 5 0.1

tupexp :: (String, Float) -> String
tupexp (str, a) = str ++ ": " ++ Prelude.show a ++ "\n"

tupcomm :: (String, (Int, [Comm])) -> String
tupcomm (str, (i, c)) = "------------------- " ++ str ++ " -------------------\nargs: " ++ Prelude.show i ++ "\ncomms: " ++ showCommList c ++ "\n"

showMap :: Show a => (Env -> Map String a) -> ((String, a) -> String) -> Env -> String
showMap f g e =
  let m = f e
      mlist = toList m
   in case mlist of
        [] -> "No hay definiciones en memoria."
        _ -> dropEnd (concatMap g mlist) 1

showVars :: Env -> String
showVars = showMap vars tupexp

showComm :: Env -> String
showComm = showMap comms tupcomm