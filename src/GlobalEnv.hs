module GlobalEnv where

import Common
import Data.Map (Map, empty)
import Graphics.Gloss
import Relude (Show)

data Env = Env
  { posx :: Float, -- Representa la posición x de la tortuga
    posy :: Float, -- Representa la posición y de la tortuga
    dir :: Float, -- Representa la dirección a donde mira la tortuga, representa los grados entre 0 y 2pi
    show :: Bool, -- Si es True, la tortuga se imprime en pantalla
    pen :: Bool, -- Si es True, los movimientos de la tortuga no dibujan
    color :: Color, -- Color del lápiz
    pics :: [Picture], -- Imagenes ya calculadas en el entorno
    vars :: Map String Exp, -- Almacena las definiciones de Variables en el entorno
    comms :: Map String (Int, [Comm]) -- Almacena las definiciones de Comandos en el entorno
  }
  deriving (Show)

defaultEnv :: Env
defaultEnv = Env 0 0 0 True False black [] empty empty