{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9  
$alpha = [a-zA-Z]  

tokens :-
$white+       ; 
  "--".*        ;
  if            { \p s -> TokenIf p}
  then          { \p s -> TokenThen p}
  else          { \p s -> TokenElse p}
  true|false    { \p s -> TokenBool p s}
  \".*\"        { \p s -> TokenString p (read s)}
  $digit+       { \p s -> TokenInt p (read s) } 
  $alpha+       { \p s -> TokenVar p s}
  \[            { \p s -> TokenLBracket p}
  \[            { \p s -> TokenRBracket p}
  \=            { \p s -> TokenEq p}
  \+            { \p s -> TokenPlus p}
  \-            { \p s -> TokenMinus p}
  \*            { \p s -> TokenMultiply p}
  \/            { \p s -> TokenDiv p}
  \^            { \p s -> TokenExp p}
  \(            { \p s -> TokenLParen p}
  \)            { \p s -> TokenRParen p}
  \:            { \p s -> TokenColon p}
  \{            { \p s -> TokenLBrace p}
  \}            { \p s -> TokenRBrace p}
  \!            { \p s -> TokenNeg p}
  \&\&          { \p s -> TokenAnd p}
  \|\|          { \p s -> TokenOr p}
  \>            { \p s -> TokenGreater p}
  \<            { \p s -> TokenLess p}
  \;            { \p s -> TokenSemi p}
  \,            { \p s -> TokenComma p}
{

data Token =  
  TokenInt AlexPosn Int     |
  TokenString AlexPosn String  | 
  TokenEq AlexPosn          |
  TokenPlus AlexPosn        |
  TokenMinus AlexPosn       |
  TokenMultiply AlexPosn       |
  TokenDiv AlexPosn         |
  TokenExp AlexPosn         |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenLBracket AlexPosn    |
  TokenRBracket AlexPosn    |
  TokenIf AlexPosn          |
  TokenThen AlexPosn        |
  TokenElse AlexPosn        |
  TokenColon AlexPosn       |
  TokenVar AlexPosn String  |
  TokenRBrace AlexPosn      |
  TokenLBrace AlexPosn      |
  TokenBool AlexPosn String |
  TokenNeg AlexPosn         |
  TokenAnd AlexPosn         |
  TokenOr AlexPosn          |
  TokenLess AlexPosn        |
  TokenGreater AlexPosn     |
  TokenSemi AlexPosn        |
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