{
module LogoPar where
import Common
import Data.Char
}

%name logoComm CommSeq
%name logoExp Exp
%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexerP } { TokenEOF }

%token
      fordward          { TokenFd }
      back              { TokenBk }
      right             { TokenRt }
      left              { TokenLt }
      clearscreen       { TokenCs }
      clean             { TokenCl }
      penup             { TokenPu }
      pendown           { TokenPd }
      hideturtle        { TokenHt }
      showturtle        { TokenSt }
      home              { TokenHome }
      setx              { TokenStx }
      sety              { TokenSty }
      setxy             { TokenStxy }
      setheading        { TokenSeth }
      repeat            { TokenRep }
      print             { TokenPrnt }
      to                { TokenTo }
      end               { TokenEnd }
      setcolor          { TokenSetCo }
      make              { TokenMk }
      for               { TokenFor }
      if                { TokenIf }
      wait              { TokenWait }
      'do.while'        { TokenDoWhile }
      while             { TokenWhile }
      skip              { TokenSkip }
      varC              { TokenVarC $$ }
      scale             { TokenScale }
      arc               { TokenArc }
      label             { TokenLabel }
      labelS            { TokenLabelS }
      

      str               { TokenStr $$ }
      '('               { TokenPA }
      ')'               { TokenPC }
      '['               { TokenCA }
      ']'               { TokenCC }


      num               { TokenNum $$ }
      xcor              { TokenXCor }
      ycor              { TokenYCor }
      heading           { TokenHead }
      towards           { TokenTow }
      varE              { TokenVarE $$ }
      readword          { TokenRead }
      random            { TokenRandom }
      '>'               { TokenMayor }
      '<'               { TokenMenor }
      '='               { TokenIgual }
      '<='              { TokenMeIgual }
      '>='              { TokenMaIgual }
      '!='              { TokenNoIgual }
      '&&'              { TokenY }
      '||'              { TokenO }
      not               { TokenNo }
      '*'               { TokenMul }
      '/'               { TokenDiv }
      '+'               { TokenSum }
      '-'               { TokenDiff }
      true              { TokenTrue }
      false             { TokenFalse }

%left '&&' '||'
%nonassoc not
%left '+' '-'
%left '*' '/'
%left NEG


%%
CommSeq :: { [Comm] }
CommSeq : CommSeqR           { reverse $1 }

CommSeqR :: { [Comm] }
CommSeqR : Comm              { [$1] }
         | CommSeqR Comm     { $2 : $1 }
         | {- empty -}       { [] }

Comm :: { Comm }
Comm : fordward Exp                                { Ford $2 }
     | back Exp                                    { Back $2 }
     | right Exp                                   { TRight $2 }
     | left Exp                                    { TLeft $2 }
     | clearscreen                                 { Clear }
     | clean                                       { Clean }
     | penup                                       { PUp }
     | pendown                                     { PDown }
     | hideturtle                                  { HideT }
     | showturtle                                  { ShowT }
     | home                                        { Home }
     | setx Exp                                    { SetX $2 }
     | sety Exp                                    { SetY $2 }
     | setxy Exp Exp                               { SetXY $2 $3 }
     | setheading Exp                              { SetHead $2 }
     | repeat Exp '[' CommSeq ']'                  { Rep $2 $4 }
     | print Exp                                   { Print $2 }
     | print str                                   { PrintStr $2 }
     | setcolor Exp                                { SetCo $2 }
     | if Exp '[' CommSeq ']' '[' CommSeq ']'      { IfC $2 $4 $7 }
     | to varC Args CommSeq end                    { Def $2 $3 $4 }
     | make str Exp                                { Save $2 $3 }
     | for '[' str Exp Exp ']' '[' CommSeq ']'     { For $3 $4 $5 $8 }
     | for '[' str Exp Exp Exp ']' '[' CommSeq ']' { ForDelta $3 $4 $5 $6 $9 }
     | wait Exp                                    { Wait $2 }
     | while Exp '[' CommSeq ']'                   { While $2 $4 }
     | 'do.while' '[' CommSeq ']' Exp              { DoWhile $3 $5 }
     | varC ExpSeq                                 { CommVar $1 $2 }
     | skip                                        { Skip }
     | scale Exp                                   { ChangeScale $2 }
     | arc Exp Exp                                 { Arco $2 $3 }
     | label str                                   { Texto $2 }
     | labelS Exp                                  { SetSizeTexto $2 }
     | '(' Comm ')'                                { $2 }

Args :: { [String] }
Args : ArgsR              { reverse $1 }

ArgsR :: { [String] }
ArgsR : str               { [$1] }
      | ArgsR str         { $2 : $1 }
      | {- empty -}       { [] }

ExpSeq :: { [Exp] }
ExpSeq : ExpSeqR          { reverse $1 }

ExpSeqR :: { [Exp] }
ExpSeqR : Exp             { [$1] }
        | ExpSeqR Exp     { $2 : $1 }
        | {- empty -}     { [] }

Exp :: { Exp }
Exp : num                  { Num $1 }
    | xcor                 { XCor }
    | ycor                 { YCor } 
    | heading              { Heading }                       
    | towards Exp Exp      { Towards $2 $3 }
    | varE                 { Var $1 }
    | Exp '+' Exp          { BinaryOp Add $1 $3 }
    | Exp '-' Exp          { BinaryOp Sub $1 $3 }
    | Exp '/' Exp          { BinaryOp Div $1 $3 }
    | Exp '*' Exp          { BinaryOp Mult $1 $3 }
    | readword             { Read }
    | if Exp Exp Exp       { IfE $2 $3 $4 }
    | Exp '>' Exp          { Compare Gt $1 $3 }
    | Exp '<' Exp          { Compare Lt $1 $3 }
    | Exp '=' Exp          { Compare Eq $1 $3 }
    | Exp '>=' Exp         { Compare GEq $1 $3 }
    | Exp '<=' Exp         { Compare LEq $1 $3 }
    | Exp '!=' Exp         { Compare Diff $1 $3 }
    | Exp '&&' Exp         { Compare And $1 $3 }
    | Exp '||' Exp         { Compare Or $1 $3 }
    | not Exp              { Not $2 }
    | true                 { T }
    | false                { F }
    | '-' Exp %prec NEG    { Negative $2 }
    | random Exp           { Random $2 }
    | '(' Exp ')'          { $2 }

{
type LineNumber = Int
type CharNumber = Int

type P a = String -> LineNumber -> CharNumber -> Either String a

getLineNo :: P LineNumber
getLineNo _ l _ = Right l

getCharNo :: P CharNumber
getCharNo _ _ c = Right c

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l c -> m s l c >>= \a -> k a s l c

returnP :: a -> P a
returnP a _ _ _ = Right a

failP :: String -> P a
failP err _ _ _ = Left err

parseError :: Token -> P a
parseError t = getLineNo `thenP`
               \line -> getCharNo `thenP`
               \char -> failP ("Error de parseo en pos (" ++
                              show line ++
                              ", " ++
                              show char ++
                              ").\nEn el token de: " ++
                              unLexer t ++
                              ".")

data Token = TokenFd
           | TokenBk 
           | TokenRt 
           | TokenLt 
           | TokenCs 
           | TokenCl
           | TokenPu
           | TokenPd 
           | TokenHt
           | TokenSt 
           | TokenHome 
           | TokenStx 
           | TokenSty 
           | TokenStxy 
           | TokenSeth
           | TokenRep 
           | TokenPrnt 
           | TokenTo 
           | TokenEnd 
           | TokenSetCo 
           | TokenMk  
           | TokenFor  
           | TokenIf
           | TokenWait 
           | TokenWhile
           | TokenDoWhile
           | TokenSkip
           | TokenXCor 
           | TokenYCor
           | TokenHead
           | TokenTow 
           | TokenRead 
           | TokenVarE String
           | TokenVarC String
           | TokenNum Float
           | TokenStr String
           | TokenPA 
           | TokenPC 
           | TokenCA 
           | TokenCC 
           | TokenMayor 
           | TokenMenor 
           | TokenIgual 
           | TokenMaIgual
           | TokenMeIgual
           | TokenNoIgual
           | TokenY
           | TokenO
           | TokenNo
           | TokenMul
           | TokenDiv
           | TokenSum
           | TokenDiff
           | TokenTrue
           | TokenFalse
           | TokenScale
           | TokenRandom
           | TokenArc
           | TokenLabel
           | TokenLabelS
           | TokenEOF
           | TokenDot

lexerP :: (Token -> P a) -> P a
lexerP cont str line char =
      case str of
            [] -> cont TokenEOF str line char
            '\n':strr -> lexerP cont strr (line + 1) 0
            _ -> let (t, strr, charr) = lexer str in cont t strr line (char + charr)

lexer :: String -> (Token, String, Int)
lexer s@(c:cs) 
      | isSpace c = let (t,ss,cc) = lexer cs in (t, ss, cc+1)
      | isAlpha c = lexVar s
      | isDigit c = lexNum s
lexer s@('.':c:cs) | isDigit c = lexNum ('0':s)
lexer ('"':cs) = let (str, rest, n) = lexStr cs
                  in (TokenStr str, rest, n+1)
lexer (':':cs) = let (str, rest, n) = lexStr cs
                  in (TokenVarE str, rest, n+1)
lexer ('>':'=':cs) = (TokenMaIgual, cs, 2)
lexer ('<':'=':cs) = (TokenMeIgual, cs, 2)
lexer ('!':'=':cs) = (TokenNoIgual, cs, 2)
lexer ('>':cs) = (TokenMayor, cs, 1)
lexer ('<':cs) = (TokenMenor, cs, 1)
lexer ('=':cs) = (TokenIgual, cs, 1)
lexer ('(':cs) = (TokenPA, cs, 1)
lexer (')':cs) = (TokenPC, cs, 1)
lexer ('[':cs) = (TokenCA, cs, 1)
lexer (']':cs) = (TokenCC, cs, 1)
lexer ('*':cs) = (TokenMul, cs, 1)
lexer ('/':cs) = (TokenDiv, cs, 1)
lexer ('+':cs) = (TokenSum, cs, 1)
lexer ('-':cs) = (TokenDiff, cs, 1)
lexer ('&':'&':cs) = (TokenY, cs, 2)
lexer ('|':'|':cs) = (TokenO, cs, 2)

lexStr cs = (str, rest, n)
            where (str, rest) = span isAlphaNum cs
                  n = length str

lexNum cs = let (num, rest) = span isDigit cs
                n = length num in
            case rest of
            '.':xs -> let (num', rest') = span isDigit xs in
                      case num' of
                        [] -> (TokenDot, rest, n + 1)
                        _ -> case rest' of -- Esta sección es por querer soportar los float sin 0
                              '.':ys -> (TokenDot, ys, n + 2)
                              _ -> (TokenNum (read $ num ++ '.' : num'), rest', n + 1 + length num')
            _ -> (TokenNum (read num), rest, n)

lexVar cs =
  case span (\x -> isAlphaNum x || x == '.') cs of
      ("fordward",rest) -> (TokenFd, rest, 8)
      ("fd",rest) -> (TokenFd, rest, 2)
      ("back",rest) -> (TokenBk, rest, 4)
      ("bk",rest) -> (TokenBk, rest, 2)
      ("right",rest) -> (TokenRt, rest, 5)
      ("rt",rest) -> (TokenRt, rest, 2)
      ("left",rest) -> (TokenLt, rest, 4)
      ("lt",rest) -> (TokenLt, rest, 2)
      ("clearscreen",rest) -> (TokenCs, rest, 11)
      ("cs",rest) -> (TokenCs, rest, 2)
      ("clean",rest) -> (TokenCl, rest, 5)
      ("penup",rest) -> (TokenPu, rest, 5)
      ("pu",rest) -> (TokenPu, rest, 2)
      ("pendown",rest) -> (TokenPd, rest, 7)
      ("pd",rest) -> (TokenPd, rest, 2)
      ("hideturtle",rest) -> (TokenHt, rest, 10)
      ("ht",rest) -> (TokenHt, rest, 2)
      ("showturtle",rest) -> (TokenSt, rest, 10)
      ("st",rest) -> (TokenSt, rest, 2)
      ("home",rest) -> (TokenHome, rest, 4)
      ("setx",rest) -> (TokenStx, rest, 4)
      ("sety",rest) -> (TokenSty, rest, 4)
      ("setxy",rest) -> (TokenStxy, rest, 5)
      ("setheading",rest) -> (TokenSeth, rest, 10)
      ("seth",rest) -> (TokenSeth, rest, 4)
      ("repeat",rest) -> (TokenRep, rest, 6)
      ("print",rest) -> (TokenPrnt, rest, 5)
      ("to",rest) -> (TokenTo, rest, 2)
      ("end",rest) -> (TokenEnd, rest, 3)
      ("setcolor",rest) -> (TokenSetCo, rest, 8)
      ("make",rest) -> (TokenMk, rest, 4)
      ("for",rest) -> (TokenFor, rest, 3)
      ("if",rest) -> (TokenIf, rest, 2)
      ("wait",rest) -> (TokenWait, rest, 4)
      ("do.while",rest) -> (TokenDoWhile, rest, 8)
      ("while",rest) -> (TokenWhile, rest, 5)
      ("skip",rest) -> (TokenSkip, rest, 5)
      ("xcor",rest) -> (TokenXCor, rest, 4)
      ("ycor",rest) -> (TokenYCor, rest, 4)
      ("heading",rest) -> (TokenHead, rest, 7)
      ("towards",rest) -> (TokenTow, rest, 7)
      ("readword",rest) -> (TokenRead, rest, 8)
      ("not",rest) -> (TokenNo, rest, 3)
      ("true",rest) -> (TokenTrue, rest, 4)
      ("false",rest) -> (TokenFalse, rest, 5)
      ("scale",rest) -> (TokenScale, rest, 5)
      ("random",rest) -> (TokenRandom, rest, 6)
      ("arc",rest) -> (TokenArc, rest, 3)
      ("label",rest) -> (TokenLabel, rest, 5)
      ("setlabelheight",rest) -> (TokenLabelS, rest, 14)
      (var,rest) -> (TokenVarC var, rest, length var)

unLexer :: Token -> String
unLexer TokenFd = "fd"
unLexer TokenBk = "bk"
unLexer TokenRt = "rt"
unLexer TokenLt = "lt"
unLexer TokenCs = "cs"
unLexer TokenCl = "clean"
unLexer TokenPu = "pu"
unLexer TokenPd = "pd"
unLexer TokenHt = "ht"
unLexer TokenSt = "st"
unLexer TokenHome = "home"
unLexer TokenStx = "setx"
unLexer TokenSty  = "sety"
unLexer TokenStxy  = "setxy"
unLexer TokenSeth = "seth"
unLexer TokenRep  = "repeat"
unLexer TokenPrnt  = "print"
unLexer TokenTo  = "to"
unLexer TokenEnd  = "end"
unLexer TokenSetCo  = "setcolor"
unLexer TokenMk   = "make"
unLexer TokenFor   = "for"
unLexer TokenIf = "if"
unLexer TokenWait  = "wait"
unLexer TokenWhile = "while"
unLexer TokenDoWhile = "do.while"
unLexer TokenSkip = "skip"
unLexer TokenXCor  = "xcor"
unLexer TokenYCor = "ycor"
unLexer TokenHead = "heading"
unLexer TokenTow  = "towards"
unLexer TokenRead  = "readword"
unLexer (TokenVarE str) = ':' : str
unLexer (TokenVarC str) = str
unLexer (TokenNum f) = show f
unLexer (TokenStr str) = '\"':str
unLexer TokenPA  = "("
unLexer TokenPC  = ")"
unLexer TokenCA  = "["
unLexer TokenCC  = "]"
unLexer TokenMayor  = ">"
unLexer TokenMenor  = "<"
unLexer TokenIgual  = "="
unLexer TokenMaIgual = ">="
unLexer TokenMeIgual = "<="
unLexer TokenNoIgual = "!="
unLexer TokenY = "&&"
unLexer TokenO = "||"
unLexer TokenNo = "not"
unLexer TokenMul = "*"
unLexer TokenDiv = "/"
unLexer TokenSum = "+"
unLexer TokenDiff = "-"
unLexer TokenTrue = "true"
unLexer TokenFalse = "false"
unLexer TokenScale = "scale"
unLexer TokenRandom = "random"
unLexer TokenArc = "arc"
unLexer TokenLabel = "label"
unLexer TokenLabelS = "setlabelheight"
unLexer TokenEOF = "Fin del archivo"
unLexer TokenDot = "\'.\'"
}