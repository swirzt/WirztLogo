module GlobalEnv where

import Common
import Data.Map (Map, empty)
import Graphics.Gloss
import Numeric.Natural (Natural)

data Env = Env
  { posx :: Float, -- Representa la posición x de la tortuga
    posy :: Float, -- Representa la posición y de la tortuga
    show :: Bool, -- Si es True, la tortuga se imprime en pantalla
    pen :: Bool, -- Si es True, los movimientos de la tortuga no dibujan
    dir :: Float, -- Representa la dirección a donde mira la tortuga, representa los grados entre 0 y 2pi
    pics :: [Picture], -- Imagenes ya calculadas en el entorno
    vars :: Map String (Exp Vars), -- Almacena las definiciones de Variables en el entorno
    comms :: Map String (Comm Vars), -- Almacena las definiciones de Comandos en el entorno
    toDis :: (Display, Color) --
  }

defaultEnv :: Display -> Color -> Env
defaultEnv d c = Env 0 0 True False (pi / 2) [] empty empty (d, c)