{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    int             { TokenInt _ _ } 
    var             { TokenVar _ _ } 
    eq              { TokenEq _ }
    acc             { TokenAccumalator _} 
    '+'             { TokenPlus _ }
    concat          { TokenConcat _}
    elemAt          { TokenElemAt _}
    '-'             { TokenMinus _ } 
    '*'             { TokenMultiply _ } 
    '^'             { TokenExp  _  }
    '/'             { TokenDiv _ } 
	  if              { TokenIf _ }
	  then            {TokenThen _ }
	  else            {TokenElse _ }
	  out             {TokenOut _ }
    --to              {TokenTo}
	  processStream   {TokenProcessStream _ }
	  head            {TokenHead _ }
    rowList         {TokenRow _ }
    seqList         {TokenSequence _}
    streams         {TokenStreams _ }
	  tail            {TokenTail _ }
	  length          {TokenLength _ }
	  string          {TokenString _ _}
	  lbracket        {TokenLBracket _}
	  rbracket        {TokenRBracket _}
	  lparen          {TokenLParen _}
	  rparen          {TokenRParen _}
	  --lbrace          {TokenLBrace _}
	  --rbrace          {TokenRBrace _}
	  boolean         {TokenBool _ _}
	  --colon           {TokenColon _}
	  neg             {TokenNeg _}
    noteq           {TokenNotEq _}
    eqeq            {TokenEqEq _}
	  and             {TokenAnd _}
	  or              {TokenOr _}
	  lt              {TokenLess _}
	  gt              {TokenGreater _}
    lteq            {TokenLessEqual _}
    gteq            {TokenGreaterEqual _}
	  semi            {TokenSemi _}
	  comma           {TokenComma _}



%left or
%left and
%nonassoc lt gt lteq gteq eq eqeq noteq
%right concat
%left '+' '-'
%left '*' '/'
%right '^'
%left neg
%left elemAt

%%

program : statement_list {Main $1}
 

func_Def : var lparen param_list rparen eq expr {Def (string $1) $3 $6 (pos $1) }


statement_list : line { [$1] }
               | line semi statement_list{ $1 : $3}
               | {--Empty--} {[]} 

line : stmt {Statement $1}
     | func_Def {Definition $1}

		  
stmt : out_stmt {OutStmt $1 }
     | process_stream_stmt {ProcessStreamStmt $1}
     | conditional_stmt {CondStmt $1}

out_stmt : out lparen expr rparen {Out $3 (pos $1)}

iteratorFromTo : math_options comma math_options comma math_options {Iterator $1 $3 $5} 


process_stream_stmt : processStream lparen expr comma expr comma expr comma iteratorFromTo rparen {ProcessStream $3 $5 $7 $9 (pos $1) }
	 
conditional_stmt : if lparen bool_options rparen then lparen stmt rparen else lparen stmt rparen {ProcCond $3 $7 $11 (pos $1) }
	 
func_stmt : var lparen param_list rparen {Call (string $1) $3 (pos $1) } 

list_params : expr comma list_params { $1 : $3 }
            | expr { [$1] }
            | {--Empty--} {[]}

param_list : list_params { ParamList $1}

conditional_expr : if lparen bool_options rparen then lparen expr rparen else lparen expr rparen {Cond $3 $7 $11 (pos $1) }
          

expr : bool_expr {BoolExpr $1}
     | math_expr {MathExpr $1}
	   | conditional_expr {CondExpr $1}
	   | list_expr {ListExpr $1}
     | list_func_expr {ListFuncExpr $1}
     | var {Var (string $1) (pos $1) }
     | out_stmt {OutExpr $1}
     | process_stream_stmt {ProcessStreamExpr $1}
     | string {StringExpr (string $1) (pos $1) }
     | acc {Accumalator (pos $1)}

list_func_expr : head lparen list_options rparen {Head $3 (pos $1)}
               | list_options elemAt math_options {ElemAt $1 $3 (pos $2)}


math_options : math_expr {OptionMathExpr $1}
             | func_stmt {MathFunc $1}
             | out_stmt {MathOut $1 }
             | process_stream_stmt {MathProcessStream $1}
             | var {MathVar (string $1) (pos $1)}
             | acc {MathAccumalator (pos $1)}


list_options : list_expr {OptionListExpr $1}
             | func_stmt {ListFunc $1}
             | var {ListVar (string $1) (pos $1) }
             | out_stmt {ListOut $1 }
             | process_stream_stmt {ListProcessStream $1}
             | acc {ListAccumalator (pos $1)}


bool_options : bool_expr {OptionBoolExpr $1}
             | func_stmt {BoolFunc $1}
             | var {BoolVar (string $1) (pos $1)}
             | out_stmt {BoolOut $1 }
             | process_stream_stmt {BoolProcessStream $1}
             | acc {BoolAccumalator (pos $1)}


bool_expr : boolean {BoolVal (stringToBool (string $1)) (pos $1) }
          | math_options eqeq math_options {MathToBool $1 Equal $3 (pos $2) }
          | math_options gteq math_options {MathToBool $1 GreaterEqual $3 (pos $2)}
          | math_options lt math_options {MathToBool $1 Less $3 (pos $2) }
          | math_options gt math_options {MathToBool $1 Greater $3 (pos $2) }
          | math_options noteq math_options {MathToBool $1 NotEqual $3 (pos $2) }
          | math_options lteq math_options {MathToBool $1 LessEqual $3 (pos $2) } 
		      | bool_options or bool_options {BoolToBool $1 Or $3 (pos $2) }
          | bool_options and bool_options {BoolToBool $1 And $3 (pos $2) }
		      | neg bool_options {Neg $2 (pos $1)}
		      | lparen bool_expr rparen {$2}



math_expr : int {IntVal (int $1) (pos $1)}
          | math_options '+' math_options {MathOp $1 Plus $3 (pos $2)}
          | math_options '-' math_options {MathOp $1 Minus $3 (pos $2)}
          | math_options '*' math_options {MathOp $1 Multiply $3 (pos $2)}
          | math_options '/' math_options {MathOp $1 Divide $3 (pos $2)}
          | math_options '^' math_options {MathOp $1 Power $3 (pos $2)}
          | length lparen list_options rparen {Length $3 (pos $1)}
		      | lparen math_expr rparen {$2}
          
list_expr : lbracket list rbracket {List $2 (pos $1) }
          | tail lparen list_options rparen {Tail $3 (pos $1) }
		      | list_options concat list_options {Concat $1 $3 (pos $2)}
          | rowList lparen math_options rparen {Row $3 (pos $1) }
          | seqList lparen math_options rparen {Sequence $3 (pos $1) }
          | streams {Streams (pos $1)}
          | lparen list_expr rparen {$2}

list : expr comma list {$1 : $3 }
     | expr { [$1] }
     | {--Empty--} {[]}

{



stringToBool :: String -> Bool
stringToBool "True" = True
stringToBool "False" = False


parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " " ++ (show t) )

data Program = Main [Line] deriving (Eq,Show)

data Line = Statement Stmt  
          | Definition Func_Def
          deriving (Eq,Show)


data Stmt = ProcessStreamStmt Process_Stream_Func
          | OutStmt Out_Func
          | CondStmt Cond_stmt
          deriving (Eq,Show)


--Iterator: (<start>, <end>, <incr value>)
data IteratorFromTo = Iterator Math_options Math_options Math_options deriving (Eq,Show)


--Process Stream: processStream(<accumalator initial>,<output functions>, <accumalator function>, IteratorFromTo)
data Process_Stream_Func = ProcessStream Expr Expr Expr IteratorFromTo AlexPosn
                         deriving (Eq,Show)

data Out_Func = Out Expr AlexPosn
              deriving (Eq,Show)

data Func_Def = Def String Param_list Expr AlexPosn
              deriving (Eq,Show)

data FuncDef_List = FuncDefList [Func_Def] AlexPosn
                 deriving (Eq,Show)

data Func_stmt = Call String Param_list AlexPosn deriving (Eq,Show)

data Cond_stmt = ProcCond Bool_options Stmt Stmt AlexPosn deriving (Eq,Show)

data Param_list = ParamList [Expr] deriving (Eq,Show)

data Expr = Func_stmt  
          | MathExpr Math_expr 
          | BoolExpr Bool_expr 
          | CondExpr Cond_expr
          | ListExpr List_expr 
          | ListFuncExpr List_Func
          | Var String AlexPosn
          | StringExpr String AlexPosn
          | ProcessStreamExpr Process_Stream_Func
          | OutExpr Out_Func
          | Accumalator AlexPosn
          deriving (Eq,Show)
 
data List_Func = Head List_options AlexPosn
               | ElemAt List_options Math_options AlexPosn
               deriving (Eq,Show)

data Bool_expr = BoolVal Bool AlexPosn
               | MathToBool Math_options Math_to_bool Math_options AlexPosn
               | BoolToBool Bool_options Bool_to_bool Bool_options AlexPosn
               | Neg Bool_options AlexPosn
               deriving (Eq,Show)

data Math_to_bool = Equal 
                  | GreaterEqual
                  | LessEqual
                  | Less
                  | Greater
                  | NotEqual
                  deriving (Eq,Show)

data Bool_to_bool = Or
                  | And 
                  deriving (Eq,Show)

data Math_expr = IntVal Int AlexPosn 
               | MathOp Math_options Math_op Math_options AlexPosn 
               | Length List_options AlexPosn
               | EOF AlexPosn
               | EOL AlexPosn
               deriving (Eq,Show)


data Math_op = Plus | Minus | Multiply | Divide | Power deriving (Eq,Show)

data Math_options = OptionMathExpr Math_expr 
                  | MathFunc Func_stmt 
                  | MathVar String AlexPosn 
                  | MathOut Out_Func
                  | MathProcessStream Process_Stream_Func
                  | MathAccumalator AlexPosn
                  deriving (Eq,Show)

data List_options = OptionListExpr List_expr 
                  | ListFunc Func_stmt
                  | ListVar String AlexPosn 
                  | ListOut Out_Func
                  | ListProcessStream Process_Stream_Func
                  | ListAccumalator AlexPosn
                  deriving (Eq,Show)

data Bool_options = OptionBoolExpr Bool_expr 
                  | BoolFunc Func_stmt 
                  | BoolVar String AlexPosn
                  | BoolOut Out_Func
                  | BoolProcessStream Process_Stream_Func
                  | BoolAccumalator AlexPosn
                  deriving (Eq,Show)

data Cond_expr = Cond Bool_options Expr Expr AlexPosn
               deriving (Eq,Show)

data List_expr = List [Expr] AlexPosn
               | Concat List_options List_options AlexPosn
               | Tail List_options AlexPosn
               | Row Math_options AlexPosn
               | Sequence Math_options AlexPosn
               | Streams AlexPosn    
               deriving (Eq,Show)




             }