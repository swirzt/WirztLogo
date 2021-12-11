module Common where

-- Considero los booleanos como el lenguaje C
-- Exp -> Float
data Exp
  = Num Float
  | XCor
  | YCor
  | Heading
  | Towards Exp Exp
  | Var String
  | BinaryOp (Float -> Float -> Float) Exp Exp
  | Read
  | IfE Exp Exp Exp
  | Access Int -- Solo usado dentro de definiciones TO y For
  -- Sección de bool
  | Compare (Float -> Float -> Bool) Exp Exp
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  | T
  | F

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
