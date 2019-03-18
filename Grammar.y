{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    int      { TokenInt _ $$ } 
    var      { TokenVar _ $$ } 
    eq       { TokenEq _ } 
    '+'      { TokenPlus _ } 
    '-'      { TokenMinus _ } 
    '*'      { TokenMultiply _ } 
    '^'      { TokenExp  _  }
    '/'      { TokenDiv _ } 
	if       { TokenIf}
	then     {TokenThen}
	else     {TokenElse}
	string   {TokenString _ $$}
	lbracket {TokenLBracket _}
	rbracket {TokenRBracket _}
	lparen   {TokenLParen _}
	rparen   {TokenRParen _}
	lbrace   {TokenLBrace _}
	rbrace   {TokenRBrace _}
	boolean  {TokenBool _ $$}
	colon    {TokenColon _}
	neg      {TokenNeg _}
	and      {TokenAnd _}
	or       {TokenOr _}
	lt       {TokenLess _}
	gt       {TokenGreater _}
	semi     {TokenSemi _}
	comma    {TokenComma _}

%%
program : stmt_list {Program $1}

stmt_list : stmt {Stmt $1}
          | stmt stmt_list {StmtList $1 $2}
		  
stmt : func_stmt {$1}
     | conditional_stmt {$1}
	 
conditional_stmt : if lparen bool_expr rparen then lparen stmt rparen else lparen stmt rparen {Cond $3 $7 $11 }
	 
func_stmt : var lparen param_list rparen {Func_stmt $1 $3}

param_list : expr {$1}
           | expr comma param_list {ParamList $1 $3}

expr : math_expr {MathExpr $1}
     | bool_expr {BoolExpr $1}
	 | conditional_expr {$1}
	 | list_expr {List_expr $1}
	 
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
          | lparen list_expr rparen {$2}
		  | list_expr '+' '+' list_expr {Concat (List_expr $1) (List_expr $4)}
		  
list : bool_list {$1}
     | math_list {$1}

bool_list : bool_expr {$1}
          | bool_expr comma bool_list {Boollist $1 $3}
		  
math_list : math_expr {$1}
          | math_expr comma math_list {Mathlist $1 $3}
		  

{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Program = Stmt_List

data Stmt_List = Stmt | StmtList Stmt Stmt_List deriving Show

data Stmt = Func_stmt | Cond_stmt deriving Show

data Func_stmt = String Param_list deriving Show

data Cond_stmt = Cond Bool_expr Stmt Stmt deriving Show

data Param_list = Expr | ParamList Expr Param_list deriving Show

data Expr = MathExpr Math_expr | BoolExpr Bool_expr | Cond_expr | List_expr deriving Show

data Bool_expr = Bool | Math_to_bool | Bool_to_bool | Neg Bool deriving Show

data Math_expr = Int | Plus | Minus | Multiply | Divide | Power deriving Show

data Cond_expr = CondExpr Bool_expr Expr Expr deriving Show

data List_expr = Bool_list | Math_list deriving Show

data Bool_list = Bool_expr | BoolList Bool_expr Bool_list deriving Show

data Math_list = Math_expr | MathList Math_expr Math_list deriving Show

data Bin_bool_op = Or | And deriving Show

data Math_bool_op = Equalto | Gteq | Lteq | Less | Greater | Noteq deriving Show

data Math_to_bool = MathToBool Math_expr Math_bool_op Math_expr deriving Show

data Bool_to_bool = BoolToBool Bool_expr Bin_bool_op Bool_expr deriving Show
}