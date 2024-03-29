{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9  
$alpha = [a-zA-Z]  
tokens :-
$white+                    ; 
  "--".*                   ;
  \\n                      ;
  if                       { \p s -> TokenIf p}
  then                     { \p s -> TokenThen p}
  else                     { \p s -> TokenElse p}
  out|OUT                  { \p s -> TokenOut p}
  EOF                      { \p s -> TokenEOF p}
  EOL                      { \p s -> TokenEOL p}
  \{                       { \p s -> TokenLBrace p}
  \}                       { \p s -> TokenRBrace p}
  processStream            { \p s -> TokenProcessStream p}
  R                        { \p s -> TokenRow p}
  S                        { \p s -> TokenSequence p}
  acc                      { \p s -> TokenAccumalator p}
  streams                  { \p s -> TokenStreams p}
  head                     { \p s -> TokenHead p}
  tail                     { \p s -> TokenTail p}
  length                   { \p s -> TokenLength p}
  True|False               { \p s -> TokenBool p s}
  \".*\"                   { \p s -> TokenString p (read s)}
  $digit+                  { \p s -> TokenInt p (read s) } 
  $alpha+                  { \p s -> TokenVar p s}
  \[                       { \p s -> TokenLBracket p}
  \]                       { \p s -> TokenRBracket p}
  \=                       { \p s -> TokenEq p}
  \=\=                     { \p s -> TokenEqEq p}
  \!\=                     { \p s -> TokenNotEq p}
  \+\+                     { \p s -> TokenConcat p}
  \+                       { \p s -> TokenPlus p}
  \-                       { \p s -> TokenMinus p}
  \*                       { \p s -> TokenMultiply p}
  \/                       { \p s -> TokenDiv p}
  \^                       { \p s -> TokenExp p}
  \(                       { \p s -> TokenLParen p}
  \)                       { \p s -> TokenRParen p}
  \:                       { \p s -> TokenColon p}
  \!                       { \p s -> TokenNeg p}
  \!\!                     { \p s -> TokenElemAt p}
  \&\&                     { \p s -> TokenAnd p}
  \|\|                     { \p s -> TokenOr p}
  \>                       { \p s -> TokenGreater p}
  \<                       { \p s -> TokenLess p}
  \>\=                     { \p s -> TokenGreaterEqual p}
  \<\=                     { \p s -> TokenLessEqual p}
  \;                       { \p s -> TokenSemi p}
  \,                       { \p s -> TokenComma p}
{


data Token = TokenInt {pos :: AlexPosn, int :: Int}       
           | TokenString {pos :: AlexPosn, string :: String}
           | TokenEq {pos :: AlexPosn}
           | TokenPlus {pos :: AlexPosn}
           | TokenMinus {pos :: AlexPosn}
           | TokenMultiply {pos :: AlexPosn}
           | TokenDiv {pos :: AlexPosn}
           | TokenExp {pos :: AlexPosn}
           | TokenLParen {pos :: AlexPosn}
           | TokenEOF {pos :: AlexPosn}
           | TokenEOL {pos :: AlexPosn}
           | TokenConcat {pos :: AlexPosn}
           | TokenRParen {pos :: AlexPosn}
           | TokenLBracket {pos :: AlexPosn}
           | TokenRBracket {pos :: AlexPosn}
           | TokenIf {pos :: AlexPosn}
           | TokenThen {pos :: AlexPosn}
           | TokenElse {pos :: AlexPosn}
           | TokenHead {pos :: AlexPosn}
           | TokenElemAt {pos :: AlexPosn}
           | TokenTail {pos :: AlexPosn}
           | TokenOut {pos :: AlexPosn}
           | TokenProcessStream {pos :: AlexPosn}
           | TokenLength {pos :: AlexPosn}
           | TokenColon {pos :: AlexPosn}
           | TokenVar {pos :: AlexPosn, string :: String} 
           | TokenEqEq {pos :: AlexPosn}
           | TokenNotEq {pos :: AlexPosn}
           | TokenRBrace {pos :: AlexPosn}
           | TokenLBrace {pos :: AlexPosn}
           | TokenBool {pos :: AlexPosn, string :: String} 
           | TokenTo {pos :: AlexPosn}
           | TokenNeg {pos :: AlexPosn}
           | TokenAnd {pos :: AlexPosn}
           | TokenStreams {pos :: AlexPosn}
           | TokenSequence {pos :: AlexPosn}
           | TokenAccumalator {pos :: AlexPosn}
           | TokenStart {pos :: AlexPosn}
           | TokenEnd {pos :: AlexPosn}
           | TokenRow {pos :: AlexPosn}
           | TokenOr {pos :: AlexPosn}
           | TokenLess {pos :: AlexPosn}
           | TokenGreater {pos :: AlexPosn}
           | TokenLessEqual {pos :: AlexPosn}
           | TokenGreaterEqual {pos :: AlexPosn}
           | TokenSemi {pos :: AlexPosn}
           | TokenComma {pos :: AlexPosn}
           deriving (Eq,Show) 
  
tokenPosn :: Token -> String
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEqEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEOF (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEOL (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
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
tokenPosn (TokenStreams (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSequence (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenProcessStream (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOut (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAccumalator (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBrace(AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTo(AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBrace (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBool (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNeg (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLess (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreater (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemi (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConcat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}