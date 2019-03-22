{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    int             { TokenInt _ $$ } 
    var             { TokenVar _ $$ } 
    eq              { TokenEq _ } 
    '+'             { TokenPlus _ }
    concat          { TokenConcat _}
    elemAt          { TokenElemAt _}
    '-'             { TokenMinus _ } 
    '*'             { TokenMultiply _ } 
    '^'             { TokenExp  _  }
    '/'             { TokenDiv _ } 
	  if              { TokenIf}
	  then            {TokenThen}
	  else            {TokenElse}
	  out             {TokenOut}
    --to              {TokenTo}
	  processStream   {TokenProcessStream}
	  head            {TokenHead}
    rowList         {TokenRow}
    seqList         {TokenSequence}
    streams         {TokenStreams}
	  tail            {TokenTail}
	  length          {TokenLength}
	  string          {TokenString _ $$}
	  lbracket        {TokenLBracket _}
	  rbracket        {TokenRBracket _}
	  lparen          {TokenLParen _}
	  rparen          {TokenRParen _}
	  lbrace          {TokenLBrace _}
	  rbrace          {TokenRBrace _}
	  boolean         {TokenBool _ $$}
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

program : lbrace stmt_list rbrace {Prog $1}
        | func_Def_List {$1}


func_Def : var lparen param_list rparen eq expr {Def $1 $3 $6 }


func_Def_List : list_func_def {FuncDefList $1}


list_func_def : func_Def { [$1] }
              | func_Def comma list_func_def { $1 : $3 }
              | {--Empty--} {[]}

stmt_list : statement_list {Statements $1}

statement_list : stmt { [$1] }
               | stmt semi stmt_list{ $1 : $3}
		  
stmt : out_stmt {$1}
     | process_stream_stmt {$1}
     | conditional_stmt {$1}

out_stmt : out lparen expr rparen {Out $3}

iteratorFromTo : math_options comma math_options comma math_options {Iterator $1 $3 $5} 

process_stream_stmt : processStream lparen expr comma expr comma iteratorFromTo rparen {ProcessStream $3 $5 $7}
	 
conditional_stmt : if lparen bool_options rparen then lparen stmt rparen else lparen stmt rparen {ProcCond $3 $7 $11 }
	 
func_stmt : var lparen param_list rparen {Call $1 $3}

param_list : expr comma param_list { $1 : $3 }
           | expr { [$1] }
           --| {--Empty--} {[]}

expr : bool_expr {BoolExpr $1}
	   | conditional_expr {$1}
	   | list_expr {$1}
     | list_func_expr {$1}
     | math_expr {MathExpr $1}
     | var {Var var}
     | out_stmt {$1}
     | process_stream_stmt {$1}
     | string {StringExpr $1}

list_func_expr : head lparen list_expr rparen {Head $3}
               | list_expr elemAt math_expr {ElemAt $1 $3}


math_options : math_expr {MathExpr $1}
             | func_stmt {MathFunc $1}
             | var {MathVar $1}


list_options : list_expr {ListExpr $1}
             | func_stmt {ListFunc $1}
             | var {ListVar $1}


bool_options : bool_expr {BoolExpr $1}
             | func_stmt {BoolFunc $1}
             | var {BoolVar $1} 


bool_expr : boolean { stringToBool (boolean) }
          | math_options eqeq math_options {$1 Equalto $3}
          | math_options gteq math_options {$1 GreaterEqual $3}
          | math_options lt math_options {$1 Less $3}
          | math_options gt math_options {$1 Greater $3}
          | math_options noteq math_options {$1 NotEqual $3}
          | math_options lteq math_options {$1 LessEqual $3} 
		      | bool_options or bool_options {$1 Or $3}
          | bool_options and bool_options {$1 And $3}
		      | neg bool_options {Neg $2}
		      | lparen bool_expr rparen {$2}


conditional_expr : if lparen bool_options rparen then lparen expr rparen else lparen expr rparen {Cond_expr $3 $7 $11}


math_expr : int {$1}
          | math_options '+' math_options {$1 Plus $3}
          | math_options '-' math_options {$1 Minus $3}
          | math_options '*' math_options {$1 Multiply $3}
          | math_options '/' math_options {$1 Divide $3}
          | math_options '^' math_options {$1 Power $3}
          | length lparen list_options rparen {Length $3}
		      | lparen math_expr rparen {$2}
          
list_expr : lbracket list rbracket {List $2}
          | tail lparen list_options rparen {Tail $3}
		      | list_options concat list_options {Concat $1 $3}
          | rowList lparen math_options rparen {Row $3}
          | seqList lparen math_options rparen {Sequence $3}
          | streams {Streams}
          | lparen list_expr rparen {$2}

list : expr comma list {$1 : $3 }
     | expr { [$1] }
     --| {--Empty--} {[]}

{

stringToBool :: String -> Bool
stringToBool "True" = True
stringToBool "False" = False


parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Program = Prog Stmt_List 
             | FuncDef_List deriving (Eq,Show)

data Stmt_List = Statements [Stmt] deriving (Eq,Show)

data Stmt = Process_Stream_Func
          | Out_Func
          | Cond_stmt
          deriving (Eq,Show)

--Iterator: (<start>, <end>, <incr value>)
data IteratorFromTo = Iterator Math_options Math_options Math_options deriving (Eq,Show)


--Process Stream: processStream(<accumalator initial>,<output functions>, <accumalator function>, IteratorFromTo)
data Process_Stream_Func = ProcessStream Expr Expr Expr IteratorFromTo
                         deriving (Eq,Show)

data Out_Func = Out Expr
              deriving (Eq,Show)

data Func_Def = Def String Param_list Expr
              deriving (Eq,Show)

data FuncDef_List = FuncDefList [Func_Def]
                 deriving (Eq,Show)

data Func_stmt = Call String Param_list deriving (Eq,Show)

data Cond_stmt = ProcCond Bool_expr Stmt Stmt deriving (Eq,Show)

data Param_list = ParamList [Expr] deriving (Eq,Show)

data Expr = Func_stmt 
          | Math_expr 
          | Bool_expr 
          | Cond_expr
          | List_expr 
          | List_Func
          | Var String
          | StringExpr String
          deriving (Eq,Show)


data List_Func = Head List_options
               | ElemAt List_options Math_options
               deriving (Eq,Show)

data Bool_expr = Bool
               | MathOptMath_options Math_to_bool Math_options 
               | Bool_options Bool_to_bool Bool_options
               | Neg Bool_options 
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

data Math_expr = Int 
               | Math_options Math_op Math_options 
               | Length List_options
               | EOF
               | EOL
               deriving (Eq,Show)


data Math_op = Plus | Minus | Multiply | Divide | Power deriving (Eq,Show)

data Math_options = MathExpr Math_expr | MathFunc Func_stmt | MathVar String deriving (Eq,Show)

data List_options = ListExpr List_expr | ListFunc Func_stmt | ListVar String deriving (Eq,Show)

data Bool_options = BoolExpr Bool_expr | BoolFunc Func_stmt | BoolVar String deriving (Eq,Show)

data Cond_expr = CondExpr Bool_expr Expr Expr 
               deriving (Eq,Show)

data List_expr = List [Expr]
               | Concat List_options List_options
               | Tail List_options
               | Row Math_options
               | Sequence Math_options
               | Streams   
               deriving (Eq,Show)
}