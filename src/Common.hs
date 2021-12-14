module Common where

-- Considero los booleanos como el lenguaje C
data NumOps = Add | Sub | Mult | Div deriving (Show)

data BoolOps = Lt | Gt | Eq | GEq | LEq | Diff | And | Or deriving (Show)

ifF :: Float -> Bool
ifF = (/= 0)

getNumOp :: NumOps -> (Float -> Float -> Float)
getNumOp Add = (+)
getNumOp Sub = (-)
getNumOp Mult = (*)
getNumOp Div = (/)

getBoolOp :: BoolOps -> (Float -> Float -> Bool)
getBoolOp Lt = (<)
getBoolOp Gt = (>)
getBoolOp Eq = (==)
getBoolOp GEq = (>=)
getBoolOp LEq = (<=)
getBoolOp Diff = (/=)
getBoolOp And = \x y -> ifF x && ifF y
getBoolOp Or = \x y -> ifF x || ifF y

data Exp
  = Num Float
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
  deriving (Show)

data Comm
  = Ford Exp
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
  | IfC Exp [Comm] [Comm]
  | Def String [String] [Comm]
  | Save String Exp
  | For String Exp Exp [Comm]
  | ForDelta String Exp Exp Exp [Comm]
  | Wait Exp
  | While Exp [Comm]
  | DoWhile [Comm] Exp
  | CommVar String [Exp]
  | Skip -- Comando que no hace nada
  deriving (Show)

data Output = Ready | GetExp | Error String | Show String

data FileType = PNG | GIF

data Input = Exit | Input String | ToFile FileType FilePath