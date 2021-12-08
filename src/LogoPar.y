{
module LogoPar where
import Common
import Data.Char
}

%name logo
%tokentype { Token }
%error { parseError }

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
      '>'               { TokenMayor }
      '<'               { TokenMenor }
      '='               { TokenIgual }
      '<='              { TokenMaIgual }
      '>='              { TokenMeIgual }
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
%nonassoc tuple
%nonassoc sum difference
%left '+' '-'
%left '*' '/'


%%
Par :: { Either Exp [Comm] }
Par : Exp          { Left $1 }
    | CommSeq      { Right $1 }

CommSeq :: { [Comm] }
CommSeq : CommSeqR           { reverse $1 }

CommSeqR :: { [Comm] }
CommSeqR : Comm              { [$1] }
         | CommSeqR Comm     { $2 : $1 }

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
     | to varC Args '[' CommSeq ']' end            { Def $2 $3 $5 }
     | make str Exp                                { Save $2 $3 }
     | for '[' str Exp Exp ']' '[' CommSeq ']'     { For $3 $4 $5 $8 }
     | for '[' str Exp Exp Exp ']' '[' CommSeq ']' { ForDelta $3 $4 $5 $6 $9 }
     | wait Exp                                    { Wait $2 }
     | while Exp '[' CommSeq ']'                   { While $2 $4 }
     | 'do.while' '[' CommSeq ']' Exp              { DoWhile $3 $5 }
     | varC ExpSeq                                 { CommVar $1 $2 }
     | skip                                        { Skip }
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

Exp :: { Exp }
Exp : num                  { Num $1 }
    | xcor                 { XCor }
    | ycor                 { YCor } 
    | heading              { Heading }                       
    | towards Exp Exp      { Towards $2 $3 }
    | varE                 { Var $1 }
    | Exp '+' Exp          { Sum $1 $3 }
    | Exp '-' Exp          { Difference $1 $3 }
    | Exp '/' Exp          { Divide $1 $3 }
    | Exp '*' Exp          { Multiply $1 $3 }
    | readword             { Read }
    | if Exp Exp Exp       { IfE $2 $3 $4 }
    | Exp '>' Exp          { Gt $1 $3 }
    | Exp '<' Exp          { Lt $1 $3 }
    | Exp '=' Exp          { Eq $1 $3 }
    | Exp '>=' Exp         { GEq $1 $3 }
    | Exp '<=' Exp         { LEq $1 $3 }
    | Exp '!=' Exp         { Diff $1 $3 }
    | Exp '&&' Exp         { And $1 $3 }
    | Exp '||' Exp         { Or $1 $3 }
    | not Exp              { Not $2 }
    | true                 { T }
    | false                { F }
    | '(' Exp ')'          { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

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
           | TokenRef 
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

lexer :: String -> [Token]
lexer = lexer' . map toLower

lexer' :: String -> [Token]
lexer' [] = []
lexer' s@(c:cs) 
      | isSpace c = lexer' cs
      | isAlpha c = lexVar s
      | isDigit c = lexNum s
lexer' s@('.':c:cs) | isDigit c = lexNum ('0':s)
lexer' ('"':cs) = let (str, rest) = lexStr cs
                  in TokenStr str : rest
lexer' (':':cs) = let (str, rest) = lexStr cs
                  in TokenVarE str : rest
lexer' ('>':'=':cs) = TokenMaIgual : lexer' cs
lexer' ('<':'=':cs) = TokenMeIgual : lexer' cs
lexer' ('!':'=':cs) = TokenNoIgual : lexer' cs
lexer' ('>':cs) = TokenMayor : lexer' cs
lexer' ('<':cs) = TokenMenor : lexer' cs
lexer' ('=':cs) = TokenIgual : lexer' cs
lexer' ('(':cs) = TokenPA : lexer' cs
lexer' (')':cs) = TokenPC : lexer' cs
lexer' ('[':cs) = TokenCA : lexer' cs
lexer' (']':cs) = TokenCC : lexer' cs
lexer' ('*':cs) = TokenMul : lexer' cs
lexer' ('/':cs) = TokenDiv : lexer' cs
lexer' ('+':cs) = TokenSum : lexer' cs
lexer' ('-':cs) = TokenDiff : lexer' cs

lexStr cs = (str, lexer' rest)
      where (str, rest) = span isAlphaNum cs

lexNum cs = TokenNum (read num) : lexer' rest
      where (num,rest) = span (\d -> isDigit d || d == '.') cs

lexVar cs =
  case span isAlphaNum cs of
      ("fordward",rest) -> TokenFd : lexer' rest
      ("fd",rest) -> TokenFd : lexer' rest
      ("back",rest) -> TokenBk : lexer' rest
      ("bk",rest) -> TokenBk : lexer' rest
      ("right",rest) -> TokenRt : lexer' rest
      ("rt",rest) -> TokenRt : lexer' rest
      ("left",rest) -> TokenLt : lexer' rest
      ("lt",rest) -> TokenLt : lexer' rest
      ("clearscreen",rest) -> TokenCs : lexer' rest
      ("cs",rest) -> TokenCs : lexer' rest
      ("clean",rest) -> TokenCl : lexer' rest
      ("penup",rest) -> TokenPu : lexer' rest
      ("pu",rest) -> TokenPu : lexer' rest
      ("pendown",rest) -> TokenPd : lexer' rest
      ("pd",rest) -> TokenPd : lexer' rest
      ("hideturtle",rest) -> TokenHt : lexer' rest
      ("ht",rest) -> TokenHt : lexer' rest
      ("showturtle",rest) -> TokenSt : lexer' rest
      ("st",rest) -> TokenSt : lexer' rest
      ("home",rest) -> TokenHome : lexer' rest
      ("setx",rest) -> TokenStx : lexer' rest
      ("sety",rest) -> TokenSty : lexer' rest
      ("setxy",rest) -> TokenStxy : lexer' rest
      ("setheading",rest) -> TokenSeth : lexer' rest
      ("seth",rest) -> TokenSeth : lexer' rest
      ("repeat",rest) -> TokenRep : lexer' rest
      ("print",rest) -> TokenPrnt : lexer' rest
      ("to",rest) -> TokenTo : lexer' rest
      ("end",rest) -> TokenEnd : lexer' rest
      ("setcolor",rest) -> TokenSetCo : lexer' rest
      ("make",rest) -> TokenMk : lexer' rest
      ("for",rest) -> TokenFor : lexer' rest
      ("if",rest) -> TokenIf : lexer' rest
      ("wait",rest) -> TokenWait : lexer' rest
      ("do.while",rest) -> TokenDoWhile : lexer' rest
      ("while",rest) -> TokenWhile : lexer' rest
      ("skip",rest) -> TokenSkip : lexer' rest
      ("xcor",rest) -> TokenXCor : lexer' rest
      ("ycor",rest) -> TokenYCor : lexer' rest
      ("heading",rest) -> TokenHead : lexer' rest
      ("towards",rest) -> TokenTow : lexer' rest
      ("readword",rest) -> TokenRead : lexer' rest
      ("not",rest) -> TokenNo : lexer' rest
      ("true",rest) -> TokenTrue : lexer' rest
      ("false",rest) -> TokenFalse : lexer' rest
      (var,rest) -> TokenVarC var : lexer' rest


}