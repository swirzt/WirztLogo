module Common where

-- Considero los booleanos como el lenguaje C
data NumOps = Add
            | Sub
            | Mult
            | Div

data BoolOps = Lt
             | Gt
             | Eq
             | GEq
             | LEq
             | Diff
             | And
             | Or

data Exp =
    Num Float
  | Negative Exp
  | XCor
  | YCor
  | Heading
  | Towards Exp Exp
  | Var String
  | BinaryOp NumOps Exp Exp
  | Read
  | IfE Exp Exp Exp
  | Access Int -- Solo usado dentro de definiciones TO y For
  | Compare BoolOps Exp Exp
  | Not Exp
  | T
  | F
  | Random Exp

data Comm =
    Ford Exp
  | Back Exp
  | TRight Exp
  | TLeft Exp
  | Clear
  | Clean
  | PUp
  | PDown
  | HideT
  | ShowT
  | Home
  | SetX Exp
  | SetY Exp
  | SetXY Exp Exp
  | SetHead Exp
  | Rep Exp [Comm]
  | Print Exp
  | PrintStr String
  | SetCo Exp
  | SetCoCustom Exp Exp Exp
  | IfC Exp [Comm] [Comm]
  | Def String [String] [Comm]
  | Save String Exp
  | For String Exp Exp [Comm]
  | ForDelta String Exp Exp Exp [Comm]
  | Wait Exp
  | While Exp [Comm]
  | DoWhile [Comm] Exp
  | CommVar String [Exp]
  | ChangeScale Exp
  | Arco Exp Exp
  | Texto String
  | SetSizeTexto Exp
  | Undo
  | Skip -- Comando que no hace nada

data Output = Ready
            | GetExp
            | Error String
            | Show String

data FileType = PNG
              | GIF

data Input = Exit
           | Input String
           | ToFile FileType FilePath
           | LoadFile FilePath
           | ListV
           | ListC

-- printers
instance Show NumOps where
  show Add = " + "
  show Sub = " - "
  show Mult = " * "
  show Div = " / "

instance Show BoolOps where
  show Lt = " < "
  show Gt = " > "
  show Eq = " = "
  show GEq = " >= "
  show LEq = " <= "
  show Diff = " != "
  show And = " && "
  show Or = " || "

concatSepWith :: [String] -> String -> String
concatSepWith [] _ = ""
concatSepWith [x] _ = x
concatSepWith (x:xs) s = x ++ s ++ concatSepWith xs s

isAtomExp :: Exp -> Bool
isAtomExp (Num _) = True
isAtomExp (Negative _) = True
isAtomExp XCor = True
isAtomExp YCor = True
isAtomExp Heading = True
isAtomExp (Var _) = True
isAtomExp Read = True
isAtomExp (Access _) = True
isAtomExp (Not _) = True
isAtomExp T = True
isAtomExp F = True
isAtomExp _ = False

showAtomExp :: Exp -> String
showAtomExp e = let se = show e
                in if isAtomExp e
                   then se
                   else "(" ++ se ++ ")"

instance Show Exp where
  show (Num f) = show f
  show (Negative e) = '-':showAtomExp e
  show XCor = "xcor"
  show YCor = "ycor"
  show Heading = "heading"
  show (Towards e1 e2) =
    concatSepWith ["tomards", showAtomExp e1, showAtomExp e2] " "
  show (Var str) = ':':str
  show (BinaryOp op e1 e2) = showAtomExp e1 ++ show op ++ showAtomExp e2
  show Read = "readword"
  show (IfE e1 e2 e3) =
    concatSepWith ["if", showAtomExp e1, showAtomExp e2, showAtomExp e3] " "
  show (Access i) = '!':show i
  show (Compare op e1 e2) = showAtomExp e1 ++ show op ++ showAtomExp e2
  show (Not e) = "not " ++ showAtomExp e
  show T = "true"
  show F = "false"
  show (Random e) = "random " ++ showAtomExp e

showCommList :: [Comm] -> String
showCommList cs =
  "[" ++ dropEnd (foldr (\c str -> show c ++ " " ++ str) "" cs) 1 ++ "]"

instance Show Comm where
  show (Ford e) = "fd " ++ showAtomExp e
  show (Back e) = "bk " ++ showAtomExp e
  show (TRight e) = "rt " ++ showAtomExp e
  show (TLeft e) = "lt " ++ showAtomExp e
  show Clear = "cs"
  show Clean = "clean"
  show PUp = "pu"
  show PDown = "pd"
  show HideT = "ht"
  show ShowT = "st"
  show Home = "home"
  show (SetX e) = "setx " ++ showAtomExp e
  show (SetY e) = "sety " ++ showAtomExp e
  show (SetXY e1 e2) =
    concatSepWith ["setxy", showAtomExp e1, showAtomExp e2] " "
  show (SetHead e) = "seth " ++ showAtomExp e
  show (Rep e cs) =
    concatSepWith ["repeat", showAtomExp e, showCommList cs] " "
  show (Print e) = "print " ++ showAtomExp e
  show (PrintStr str) = "print \"" ++ str
  show (SetCo e) = "setcolor " ++ showAtomExp e
  show (SetCoCustom e1 e2 e3) = concatSepWith
    ["setcustomcolor", showAtomExp e1, showAtomExp e2, showAtomExp e3]
    " "
  show (IfC e cs1 cs2) =
    concatSepWith ["if", showAtomExp e, showCommList cs1, showCommList cs2] " "
  show (Def str ss cs) = concatSepWith
    ["to", str, concatMap (\s -> '\"':s ++ " ") ss ++ showCommList cs]
    " "
  show (Save str e) = "make \"" ++ str ++ " " ++ showAtomExp e
  show (For str e1 e2 cs) = concatSepWith
    ["for", "[\"" ++ str, showAtomExp e1, showAtomExp e2, "]", showCommList cs]
    " "
  show (ForDelta str e1 e2 e3 cs) = concatSepWith
    [ "for"
    , "[\"" ++ str
    , showAtomExp e1
    , showAtomExp e2
    , showAtomExp e3
    , "]"
    , showCommList cs]
    " "
  show (Wait e) = "wait " ++ showAtomExp e
  show (While e cs) =
    concatSepWith ["while", showAtomExp e, showCommList cs] " "
  show (DoWhile cs e) =
    concatSepWith ["do.while", showCommList cs, showAtomExp e] " "
  show (CommVar str es) =
    str ++ " " ++ dropEnd (foldr (\e st -> showAtomExp e ++ " " ++ st) "" es) 1
  show (ChangeScale e) = "scale " ++ showAtomExp e
  show (Arco e1 e2) = concatSepWith ["arc", showAtomExp e1, showAtomExp e2] " "
  show (Texto str) = "label \"" ++ str
  show (SetSizeTexto e) = "setlabelheight " ++ showAtomExp e
  show Undo = "undo"
  show Skip = "skip"

dropEnd :: [a] -> Int -> [a]
dropEnd l n = snd $ dropEnd' n l
  where
    dropEnd' :: Int -> [a] -> (Int, [a])
    dropEnd' p [] = (p, [])
    dropEnd' p (x:xs) = case dropEnd' p xs of
      (0, ys) -> (0, x:ys)
      (m, []) -> (m - 1, [])
      _       -> undefined -- si m > 0 entonces la lista es vacía
