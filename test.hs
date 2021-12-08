import Graphics.Gloss.Interface.IO.Animate (Controller (controllerSetRedraw))
import Graphics.Gloss.Interface.IO.Display (Display (InWindow), blank, displayIO, white)

test :: IO ()
test = displayIO (InWindow "" (100, 100) (10, 10)) white (return blank) controllerSetRedraw