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
    '-'             { TokenMinus _ } 
    '*'             { TokenMultiply _ } 
    '^'             { TokenExp  _  }
    '/'             { TokenDiv _ } 
	if              { TokenIf}
	then            {TokenThen}
	else            {TokenElse}
	out             {TokenOut}
	processStream   {TokenProcessStream}
	head            {TokenHead}
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
	colon           {TokenColon _}
	neg             {TokenNeg _}
	and             {TokenAnd _}
	or              {TokenOr _}
	lt              {TokenLess _}
	gt              {TokenGreater _}
	semi            {TokenSemi _}
	comma           {TokenComma _}

%%
program : stmt_list {Program $1}


func_Def : var lparen param_list rparen eq expr {Def $1 $3 $6 }
func_Def_List : func_Def { $1 }
              | func_Def comma func_Def_List {DefElem $1 $3 }

stmt_list : stmt {$1}
          | stmt_list semi stmt{Line $1 $3}
		  
stmt : out_stmt {$1}
     | conditional_stmt {$1}
     | process_stream_stmt {$1}

out_stmt : out lparen expr rparen {$3}

process_stream_stmt : pr


	 
conditional_stmt : if lparen bool_expr rparen then lparen stmt_list rparen else lparen stmt_list rparen {ProcCond $3 $7 $11 }
	 
func_stmt : var lparen param_list rparen {Call $1 $3}

param_list : expr {$1}
           | expr comma param_list {Param $1 $3}

expr : math_expr {MathExpr $1}
     | bool_expr {BoolExpr $1}
	 | conditional_expr {$1}
	 | list_expr {$1}
     | list_func_expr {ListFunc $1}

list_func_expr : head list_expr {Head (List_expr $1)}
               | elemAt list_expr math_expr {ElemAt (List_expr $1) (Math_expr $1)}
	 
bool_expr : boolean {Bool boolean}
          | math_expr math_bool_op math_expr {MathToBool $1 $2 $3}
		  | bool_expr bin_bool_op bool_expr {BoolToBool (BoolExpr $1) (Bin_bool_op $2) (BoolExpr $3)}
		  | neg bool_expr {Neg $2}
		  | lparen bool_expr rparen {$2}

math_bool_op : eq eq {Equalto}
             | gt eq {Gteq}
			 | lt eq {Lteq}
			 | lt {Less}
			 | gt {Greater}
			 | neg eq {Noteq}
			
bin_bool_op : or {Or}
            | and {And}

conditional_expr : if lparen bool_expr rparen then lparen expr rparen else lparen expr rparen {Cond_expr (BoolExpr $3) (Expr $7) (Expr $11)}

math_expr : int {Int $1}
          | math_expr '+' math_expr {Plus $1 $3}
          | math_expr '-' math_expr {Minus $1 $3}
		  | math_expr '*' math_expr {Multiply $1 $3}
		  | math_expr '/' math_expr {Divide $1 $3}
		  | math_expr '^' math_expr {Power $1 $3}
		  | lparen math_expr rparen {$2}

list_expr : lbracket list rbracket {$2}
          | tail list_expr {Tail $2}
          | lparen list_expr rparen {$2}
		  | list_expr '+' '+' list_expr {Concat (List_expr $1) (List_expr $4)}


		  
list : bool_list {$1}
     | math_list {$1}

bool_list : bool_expr {$1}
          | bool_expr comma bool_list {BoolElem $1 $3}
		  
math_list : math_expr {$1}
          | math_expr comma math_list {MathElem $1 $3}
		  

{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Program = Stmt_List deriving (Eq,Show)



data Stmt_List = Stmt | Line Stmt Stmt_List deriving (Eq,Show)

data Stmt = Process_Stream_Func
          | Out_Func
          | Cond_stmt
          deriving (Eq,Show)

data Process_Stream_Func = ProcessStream Expr Expr Expr
                         deriving (Eq,Show)

data Out_Func = Out Expr
              deriving (Eq,Show)

data Func_Def = Def String Param_list Expr
              deriving (Eq,Show)

data FuncDef_List = Func_Def | DefElem Func_Def FucDef_List
                 deriving (Eq,Show)

data Func_stmt = Call String Param_list deriving (Eq,Show)

data Cond_stmt = ProcCond Bool_expr Stmt_List Stmt_List deriving (Eq,Show)

data Param_list = Func_stmt | Param Expr Param_list deriving (Eq,Show)

data Expr = Cond_stmt 
          | Func_stmt 
          | MathExpr Math_expr 
          | BoolExpr Bool_expr 
          | Cond_expr
          | List_expr 
          | Head List_expr
          | ElemAt List_expr Math_expr
          deriving (Eq,Show)

data Bool_expr = BoolVar String 
               | Bool 
               | Math_to_bool 
               | Bool_to_bool 
               | Neg Bool 
               deriving (Eq,Show)

data Math_expr = MathVar String 
               | Int 
               | Plus Math_expr Math_expr 
               | Minus Math_expr Math_expr  
               | Multiply Math_expr Math_expr  
               | Divide Math_expr Math_expr  
               | Power Math_expr Math_expr 
               | Length List_expr  
               deriving (Eq,Show)

data Cond_expr = CondExpr Bool_expr Expr Expr 
               deriving (Eq,Show)

data List_expr = ListVar String 
               | Bool_list 
               | Math_list
               | Concat List_expr List_expr 
               deriving (Eq,Show)

data Bool_list = ListBool String 
               | Bool_expr 
               | BoolList Bool_expr Bool_list 
               | Bool_list
               deriving (Eq,Show)


data Math_list = Math_expr 
               | MathList Math_expr Math_list
               | Math_list
               deriving (Eq,Show)


data Math_to_bool = Equal Math_expr Math_expr 
                  | GreaterEqual Math_expr Math_expr
                  | LessEqual Math_expr Math_expr
                  | Less Math_expr Math_expr
                  | Greater Math_expr Math_expr
                  | NotEqual Math_expr Math_expr 
                  deriving (Eq,Show)

data Bool_to_bool = Or Bool_expr Bool_expr 
                  | And Bool_expr Bool_expr 
                  deriving (Eq,Show)


