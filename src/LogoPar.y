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
      fill              { TokenFill }
      filled            { TokenFilled }
      wait              { TokenWait }
      'do.while'        { TokenWhile }
      
      pos               { TokenPos }
      list              { TokenList }
      tuple             { TokenTup }
      
      xcor              { TokenXCor }
      ycor              { TokenYCor }
      heading           { TokenHead }
      towards           { TokenTow }
      ':'               { TokenRef }
      sum               { TokenSum }
      difference        { TokenDiff }
      first             { TokenFst }
      last              { TokenLst }
      item              { TokenItem }
      pick              { TokenPick }
      butfirst          { TokenTail }
      butlast           { TokenRTail }
      readword          { TokenRead }

      varE              { TokenVarE $$ }
      varC              { TokenVarC $$ }
      num               { TokenNum $$ }
      str               { TokenStr $$ }
      '('               { TokenPA }
      ')'               { TokenPC }
      '['               { TokenCA }
      ']'               { TokenCC }


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
      '+'               { TokenSSum }
      '-'               { TokenSDiff }

%left '&&' '||'
%nonassoc not
%nonassoc tuple
%nonassoc sum difference
%left '+' '-'
%left '*' '/'


%%
CommExp :: { Either (Exp String) [Comm String] }
CommExp : CommSeq           { Right $1 }
        | Exp               { Left $1 }

CommSeq :: { [Comm String] }
CommSeq : Comm              { [$1] }
        | Comm CommSeq      { $1 : $2 }

Comm :: { Comm String }
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
     | to varC Args CommSeq end                    { Def $2 $3 $4 }
     | setcolor Exp                                { SetCo $2 }
     | make str Exp                                { DefV $2 $3 }
     | for List '[' CommSeq ']'                    { For $3 $4 $5 $8 }
     | if Bool '[' CommSeq ']'                     { IfC $2 $4 }
     | fill                                        { Fill }
     | filled Exp '[' CommSeq ']'                  { Filled $2 $4 }
     | wait Exp                                    { Wait $2 }
     | 'do.while' '[' CommSeq ']' Bool             { While $3 $5 }
     | varC List                                   { CommVar $1 $2 }
     | '(' Comm ')'                                { $2 }

Exp :: { Exp String }
Exp : num                  { Num $1 }
    | xcor                 { XCor }
    | ycor                 { YCor } 
    | heading              { Heading }                       
    | towards Exp          { Towards $2 }
    | varE                 { Var $1 }
    | sum Exp Exp          { Sum $2 $3 }
    | difference Exp Exp   { Difference $2 $3 }
    | first Exp            { First $2 }
    | last Exp             { Last $2 }
    | item Exp Exp         { Item $2 $3 }
    | pick Exp             { RandItem $2 }
    | butfirst Exp         { Tail $2 }
    | butlast Exp          { RTail $2 }
    | readword             { Read }
    | Exp '+' Exp          { Sum $1 $3 }
    | Exp '-' Exp          { Difference $1 $3 }
    | Exp '/' Exp          { Divide $1 $3 }
    | Exp '*' Exp          { Multiply $1 $3 }
    | str                  { Str $1 }
    | List                 { EList $1 }
    | if Bool List         { IfE $2 $3 }
    | '(' Exp ')'          { $2 }


EL :: { Either (List String) (Exp String) }
EL : Exp                  { Right $1 }
   | List                 { Left $1 }

List :: { List String }
List : pos                { Pos }
     | tuple EL EL        { L [$2, $3] }
     | '[' Seq ']'        { L $2 }
     | '(' list Seq ')'   { L $3 }

Seq :: { [Either (List String) (Exp String)] }
Seq : EL              { [$1] }
    | EL Seq          { $1 : $2 }

Args :: { [String] }
Args : varE        { [$1] }
     | varE Args   { $1 : $2 }

Bool :: { Boolen String }
Bool : Exp '>' Exp     { Gt $1 $3 }
     | Exp '<' Exp     { Lt $1 $3 }
     | Exp '=' Exp     { Eq $1 $3 }
     | Exp '>=' Exp    { GEq $1 $3 }
     | Exp '<=' Exp    { LEq $1 $3 }
     | Exp '!=' Exp    { Diff $1 $3 }
     | Bool '&&' Bool  { And $1 $3 }
     | Bool '||' Bool  { Or $1 $3 }
     | not Bool        { Not $2 }
     | '(' Bool ')'    { $2 }

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
           | TokenFill  
           | TokenFilled
           | TokenWait 
           | TokenWhile
           | TokenPos 
           | TokenList
           | TokenTup
           | TokenXCor 
           | TokenYCor 
           | TokenHead 
           | TokenTow 
           | TokenRef 
           | TokenSum 
           | TokenDiff 
           | TokenFst 
           | TokenLst 
           | TokenItem 
           | TokenPick 
           | TokenTail 
           | TokenRTail
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
           | TokenSSum
           | TokenSDiff

lexer :: String -> [Token]
lexer = lexer' . map toLower

lexer' :: String -> [Token]
lexer' [] = []
lexer' (c:cs) 
      | isSpace c = lexer' cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
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
lexer' ('(':cs) = case lexer' cs of
                    TokenTup:xs -> TokenPA : TokenList : xs
                    xs -> TokenPA : xs
lexer' (')':cs) = TokenPC : lexer' cs
lexer' ('[':cs) = TokenCA : lexer' cs
lexer' (']':cs) = TokenCC : lexer' cs
lexer' ('*':cs) = TokenMul : lexer' cs
lexer' ('/':cs) = TokenDiv : lexer' cs
lexer' ('+':cs) = TokenSSum : lexer' cs
lexer' ('-':cs) = TokenSDiff : lexer' cs

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
      ("fill",rest) -> TokenFill : lexer' rest
      ("filled",rest) -> TokenFilled : lexer' rest
      ("wait",rest) -> TokenWait : lexer' rest
      ("do.while",rest) -> TokenWhile : lexer' rest
      ("pos",rest) -> TokenPos : lexer' rest
      ("list",rest) -> TokenTup : lexer' rest
      ("xcor",rest) -> TokenXCor : lexer' rest
      ("ycor",rest) -> TokenYCor : lexer' rest
      ("heading",rest) -> TokenHead : lexer' rest
      ("towards",rest) -> TokenTow : lexer' rest
      ("sum",rest) -> TokenSum : lexer' rest
      ("difference",rest) -> TokenDiff : lexer' rest
      ("first",rest) -> TokenFst : lexer' rest
      ("last",rest) -> TokenLst : lexer' rest
      ("item",rest) -> TokenItem : lexer' rest
      ("pick",rest) -> TokenPick : lexer' rest
      ("butfirst",rest) -> TokenTail : lexer' rest
      ("butlast",rest) -> TokenRTail : lexer' rest
      ("readword",rest) -> TokenRead : lexer' rest
      ("not",rest) -> TokenNo : lexer' rest
      (var,rest)   -> TokenVarC var : lexer' rest


}