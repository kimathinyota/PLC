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
     | func_stmt {FuncStmt $1}
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
               | lparen list_func_expr rparen {$2}


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

data Program = Main { lines :: [Line]} deriving (Eq,Show)

data Line = Statement { stmt :: Stmt}  
          | Definition { def :: Func_Def}
          deriving (Eq,Show)


data Stmt = ProcessStreamStmt { processStream :: Process_Stream_Func}
          | OutStmt { out :: Out_Func}
          | CondStmt { cond :: Cond_stmt}
          deriving (Eq,Show)


--Iterator: (<start>, <end>, <incr value>)
data IteratorFromTo = Iterator { start :: Math_options, end :: Math_options, incr :: Math_options} deriving (Eq,Show)


--Process Stream: processStream(<accumalator initial>,<output functions>, <accumalator function>, IteratorFromTo)
data Process_Stream_Func = ProcessStream {init::Expr, output::Expr, acc :: Expr , iterator::IteratorFromTo,  processStreamPos::AlexPosn}
                         deriving (Eq,Show)

data Out_Func = Out {expr::Expr, outPos::AlexPosn}
              deriving (Eq,Show)

data Func_Def = Def { defName::String, defParams::Param_list, declr::Expr, defPos::AlexPosn}
              deriving (Eq,Show)

data FuncDef_List = FuncDefList {defs::[Func_Def], defListPos::AlexPosn}
                 deriving (Eq,Show)

data Func_stmt = Call {funcName::String, params::Param_list, callPos::AlexPosn} deriving (Eq,Show)

data Cond_stmt = ProcCond {stmtCond::Bool_options, trueStmt::Stmt, falseStmt::Stmt, condStmtPos::AlexPosn} deriving (Eq,Show)

data Param_list = ParamList { paramlist::[Expr]} deriving (Eq,Show)

data Expr = FuncStmt Func_stmt  
          | MathExpr {mexpr::Math_expr} 
          | BoolExpr {bexpr::Bool_expr} 
          | CondExpr {cexpr::Cond_expr}
          | ListExpr {lexpr::List_expr }
          | ListFuncExpr {listFuncExpr::List_Func}
          | Var {var::String, varPos::AlexPosn}
          | StringExpr {nameString::String, stringPos::AlexPosn}
          | ProcessStreamExpr {processStreamExpr::Process_Stream_Func}
          | OutExpr {outExpr::Out_Func}
          | Accumalator {accPos::AlexPosn}
          deriving (Eq,Show)
 
data List_Func = Head {listHead::List_options, headPos::AlexPosn}
               | ElemAt {listInput::List_options, atMath::Math_options, listFuncPos::AlexPosn}
               deriving (Eq,Show)

data Bool_expr = BoolVal {boolVal::Bool, boolPos::AlexPosn}
               | MathToBool {mathA::Math_options, mathToBoolOp::Math_to_bool, mathB::Math_options, mathToBoolPos::AlexPosn}
               | BoolToBool {boolA::Bool_options, boolToBoolOp::Bool_to_bool, boolB::Bool_options, boolToBoolPos::AlexPosn}
               | Neg {boolNeg::Bool_options, posNeg::AlexPosn}
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

data Math_expr = IntVal {intVal::Int, intPos::AlexPosn} 
               | MathOp {mathOne::Math_options, mathOp::Math_op, mathTwo::Math_options, opPos::AlexPosn} 
               | Length {listLength::List_options, lengthPos::AlexPosn}
               | EOF {eofPos::AlexPosn}
               | EOL {eolPos::AlexPosn}
               deriving (Eq,Show)


data Math_op = Plus | Minus | Multiply | Divide | Power deriving (Eq,Show)

data Math_options = OptionMathExpr {mathExpr::Math_expr} 
                  | MathFunc {mathFunc::Func_stmt} 
                  | MathVar {mathVar::String, mathVarPos::AlexPosn} 
                  | MathOut {mathOut::Out_Func}
                  | MathProcessStream {mathPS::Process_Stream_Func}
                  | MathAccumalator {mathAccPos::AlexPosn}
                  deriving (Eq,Show)

data List_options = OptionListExpr {listExpr::List_expr }
                  | ListFunc {listFunc::Func_stmt}
                  | ListVar {listVar::String, listVarPos::AlexPosn} 
                  | ListOut {listOut::Out_Func}
                  | ListProcessStream {listPS::Process_Stream_Func}
                  | ListAccumalator {listAccPos::AlexPosn}
                  deriving (Eq,Show)

data Bool_options = OptionBoolExpr {boolExpr::Bool_expr} 
                  | BoolFunc {boolFunc::Func_stmt} 
                  | BoolVar {boolVar::String, boolVarPos::AlexPosn}
                  | BoolOut {boolOut::Out_Func}
                  | BoolProcessStream {boolPS::Process_Stream_Func}
                  | BoolAccumalator {boolAccPos::AlexPosn}
                  deriving (Eq,Show)

data Cond_expr = Cond {condExpr::Bool_options, trueExpr::Expr, falseExpr::Expr, condExprPos::AlexPosn}
               deriving (Eq,Show)

data List_expr = List { list::[Expr], listPos::AlexPosn}
               | Concat {listA::List_options, listB::List_options, concatPos::AlexPosn}
               | Tail {listTail::List_options, tailPos::AlexPosn}
               | Row {rowMath::Math_options, rowPos::AlexPosn}
               | Sequence {seqMath::Math_options, seqPos::AlexPosn}
               | Streams {streamPos::AlexPosn}    
               deriving (Eq,Show)




             }