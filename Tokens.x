{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9  
$alpha = [a-zA-Z]  
$processStream = (p|P)(r|R)(o|O)(c|C)(e|E)(s|S)(s|S)(S|s)(t|T)(r|R)(e|E)(a|A)(m|M)
tokens :-
$white+       ; 
  "--".*        ;
  if                       { \p s -> TokenIf p}
  then                     { \p s -> TokenThen p}
  else                     { \p s -> TokenElse p}
  out|OUT                  { \p s -> TokenOut p}
  $processStream           { \p s -> TokenProcessStream p}
  head                     { \p s -> TokenHead p}
  tail                     { \p s -> TokenTail p}
  length                   { \p s -> TokenLength p}
  True|False               { \p s -> TokenBool p (stringToBool s)}
  \".*\"                   { \p s -> TokenString p (read s)}
  $digit+                  { \p s -> TokenInt p (read s) } 
  $alpha+                  { \p s -> TokenVar p s}
  \[                       { \p s -> TokenLBracket p}
  \[                       { \p s -> TokenRBracket p}
  \=                       { \p s -> TokenEq p}
  \+                       { \p s -> TokenPlus p}
  \-                       { \p s -> TokenMinus p}
  \*                       { \p s -> TokenMultiply p}
  \/                       { \p s -> TokenDiv p}
  \^                       { \p s -> TokenExp p}
  \(                       { \p s -> TokenLParen p}
  \)                       { \p s -> TokenRParen p}
  \:                       { \p s -> TokenColon p}
  \{                       { \p s -> TokenLBrace p}
  \}                       { \p s -> TokenRBrace p}
  \!                       { \p s -> TokenNeg p}
  \&\&                     { \p s -> TokenAnd p}
  \|\|                     { \p s -> TokenOr p}
  \>                       { \p s -> TokenGreater p}
  \<                       { \p s -> TokenLess p}
  \;                       { \p s -> TokenSemi p}
  \,                       { \p s -> TokenComma p}
{


stringToBool :: String -> Bool
stringToBool "True" = True
stringToBool "False" = False

data Token =  
  TokenInt AlexPosn Int       |
  TokenString AlexPosn String | 
  TokenEq AlexPosn            |
  TokenPlus AlexPosn          |
  TokenMinus AlexPosn         |
  TokenMultiply AlexPosn      |
  TokenDiv AlexPosn           |
  TokenExp AlexPosn           |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenLBracket AlexPosn      |
  TokenRBracket AlexPosn      |
  TokenIf AlexPosn            |
  TokenThen AlexPosn          |
  TokenElse AlexPosn          |
  TokenHead AlexPosn          |
  TokenTail AlexPosn          |
  TokenOut AlexPosn           |
  TokenProcessStream AlexPosn |
  TokenLength AlexPosn        |
  TokenColon AlexPosn         |
  TokenVar AlexPosn String    |
  TokenRBrace AlexPosn        |
  TokenLBrace AlexPosn        |
  TokenBool AlexPosn String   |
  TokenNeg AlexPosn           |
  TokenAnd AlexPosn           |
  TokenOr AlexPosn            |
  TokenLess AlexPosn          |
  TokenGreater AlexPosn       |
  TokenSemi AlexPosn          |
  TokenComma AlexPosn
  deriving (Eq,Show) 
  
tokenPosn :: Token -> String
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExp(AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHead (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTail (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenProcessStream (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOut (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBrace(AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBrace (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBool (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNeg (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLess (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreater (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemi (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}