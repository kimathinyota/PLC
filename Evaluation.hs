
module Evaluation where 
import Tokens
import Grammar
import System.IO

data Error a = Valid a | Errors [String] deriving (Eq,Show)

instance Functor Error where
  fmap f (Errors x) = Errors x
  fmap f (Valid x)  = Valid (f x)

instance Applicative Error where
  pure a                 = Valid a
  (Valid f) <*> Valid x  = Valid (f x)
  (Valid f) <*> Errors x = Errors x

instance Monad Error where
  (Errors x)        >>= f = (Errors x)
  (Valid x)         >>= f = f x
  return x                = Valid x

invalid (Valid x) = False
invalid (Errors _) = True

showPos :: AlexPosn -> String
showPos (AlexPn a l c)  = "line:column " ++ show(l) ++ ":" ++ show(c)

isFuncDef :: Line -> Bool
isFuncDef (Definition _) = True
isFuncDef (_) = False

getDefinition :: Line -> Func_Def
getDefinition (Definition x) = x

getStatement :: Line -> Stmt
getStatement (Statement x) = x

getFunctionDefinitions :: [Line] -> [Func_Def]
getFunctionDefinitions [] = []
getFunctionDefinitions (x:xs) | (isFuncDef x)==True = getDefinition(x) : (getFunctionDefinitions xs)
                              | otherwise = (getFunctionDefinitions xs)

getStatements :: [Line] -> [Stmt]
getStatements [] = []
getStatements (x:xs) | (isFuncDef x)==False = (getStatement x) : (getStatements xs)
                     | otherwise = (getStatements xs)

data Value = IntegerValue {valInt::Int}
           | BooleanValue {valBool::Bool} 
           | ListValue {listVal::[Value] }
           | Null {errors::[String]}
           deriving (Eq,Show)


data CallVariable = Variable {name::String, value::Value} deriving (Eq,Show)


evaluateProgram :: Program -> [Value]
evaluateProgram (Main lines) = let stmts = (getStatements lines)
                                   defs = (getFunctionDefinitions lines)
                               in [ (evaluateStmt defs [] x) | x <- stmts]
             



evaluateStmt :: [Func_Def] -> [CallVariable] ->  Stmt -> (Value)
evaluateStmt defs vars (ProcessStreamStmt x) = evaluateProcessStreamStmt defs vars x
evaluateStmt defs vars (OutStmt x) = evaluateOutStmt defs vars x
evaluateStmt defs vars (CondStmt x) = evaluateCondStmt defs vars x

readInt x = (read x) :: Int
getRow x = takeWhile( \x->not(x=='\n')  ) x
getInts :: String -> [Int]
getInts (word) = [ readInt(x:"") | x <- getRow(word) , not(x==' ')]


handleProcessStream stream count endCount = do done <- isEOF
                                               if (done || (count>=endCount) ) 
                                                  then return "Test"
                                                  else (do row <- getLine
                                                           handleProcessStream (stream ++ [(getInts row)]) (count+1) endCount )

evaluateProcessStreamStmt :: [Func_Def] -> [CallVariable] -> Process_Stream_Func -> (Value) 
evaluateProcessStreamStmt defs vars processStreamFunc = Null [""]

evaluateOutStmt :: [Func_Def] -> [CallVariable] -> Out_Func -> (Value) 
evaluateOutStmt defs vars (Out expr _) = evaluateExpr defs vars expr

evaluateCondStmt :: [Func_Def] -> [CallVariable] -> Cond_stmt -> (Value) 
evaluateCondStmt defs vars condStmt = let cond = (evaluateBoolOptions defs vars (stmtCond condStmt)) in
                                          (case cond of 
                                                (BooleanValue True) -> (evaluateStmt defs vars (trueStmt condStmt)  )
                                                (BooleanValue False) -> (evaluateStmt defs vars (falseStmt condStmt) )
                                                (Null errors) -> Null (errors ++ [( "Invalid condition for if statement at  " ++ showPos(condStmtPos(condStmt)) ) ]) )


getValueOfVariable :: [CallVariable] -> String -> AlexPosn -> Value
getValueOfVariable [] var pos = Null [("Invalid identifier at " ++ (showPos pos))]
getValueOfVariable (x:xs) var pos | (name x)==var = (value x)
                                  | otherwise = (getValueOfVariable xs var pos)



evaluateExpr :: [Func_Def] -> [CallVariable] -> Expr -> (Value)
evaluateExpr defs vars (FuncStmt func) = evaluateFuncStmt defs vars func
evaluateExpr defs vars (MathExpr mexpr) = evaluateMexpr defs vars mexpr
evaluateExpr defs vars (CondExpr cexpr) = evaluateCexpr defs vars cexpr
evaluateExpr defs vars (BoolExpr bexpr) = evaluateBexpr defs vars bexpr
evaluateExpr defs vars (ListExpr lexpr) = evaluateLexpr defs vars lexpr
evaluateExpr defs vars (ListFuncExpr func) = evaluateListFuncExpr defs vars func
evaluateExpr defs vars (Var x pos) = getValueOfVariable vars x pos  
evaluateExpr defs vars (OutExpr outStmt) = (evaluateOutStmt defs vars outStmt)
evaluateExpr defs vars (ProcessStreamExpr func) = evaluateProcessStreamStmt defs vars func
evaluateExpr defs vars (StringExpr x pos) = Null [("Invalid identifier " ++ x ++ " at " ++ (showPos pos))]
evaluateExpr defs vars (Accumalator pos) = Null [("Accumalator can only be accessed within processStream function call. Error at " ++ (showPos pos))]




getFuncDef :: [Func_Def] -> String -> AlexPosn -> (Error Func_Def)




getFuncDef [] callName pos =  Errors ["Function doesn't exist. Error at " ++(showPos pos) ]
getFuncDef (x:xs) callName pos | (defName x)==callName = return x
                               | otherwise = (getFuncDef xs callName pos)



returnCallVariables :: [Func_Def] -> [CallVariable] -> Func_Def -> Param_list -> Error [CallVariable]

returnCallVariables defs vars (Def n (ParamList [] ) _ _ ) (ParamList []) = return []

returnCallVariables defs vars (Def n (ParamList ((Var x _):xs)) e pos) (ParamList (paramExpr:ys)) = do let val = evaluateExpr defs vars paramExpr
                                                                                                       case val of 
                                                                                                            (Null errors) -> Errors (errors ++ ["Error in given parameter at "++(showPos pos)])
                                                                                                            (_) -> (do cvars <- (returnCallVariables defs vars (Def n (ParamList xs) e pos) (ParamList ys)) 
                                                                                                                       return ( (Variable x val) : cvars ) )
                                                                                                       

returnCallVariables defs vars (Def n (ParamList (_:xs)) e pos) (ParamList (y:ys)) = Errors["Invalid function definition at " ++ (showPos pos)]    



evaluateFuncStmt :: [Func_Def] -> [CallVariable] -> Func_stmt -> (Value)
evaluateFuncStmt defs vars (Call name params pos) = let defErr = getFuncDef defs name pos in
                                                        (case defErr of 
                                                              (Valid def)  -> (if (length (paramlist (defParams def)) == length (paramlist (params)))
                                                                                            then (let callVariables = returnCallVariables defs vars def params in
                                                                                                      case callVariables of 
                                                                                                           (Valid callVar) -> evaluateExpr defs callVar (declr def)
                                                                                                           (Errors errors) -> Null errors )
                                                                                            else Null ["An invalid number of parameters given to function call at " ++ (showPos pos)])
                                                              (Errors errors) -> Null errors )





evaluateMexpr :: [Func_Def] -> [CallVariable] -> Math_expr -> (Value)
evaluateMexpr defs vars (IntVal int pos) = (IntegerValue int)
evaluateMexpr defs vars (Length list pos) = let lst = evaluateListOptions defs vars list in
                                                (case lst of 
                                                      (ListValue list) -> (IntegerValue (length(list)) )
                                                      (Null err) -> Null (err ++ ["Invalid list given to length function at "++ showPos(pos)]) )

evaluateMexpr defs vars (EOF pos) =  Null [("EOF can only be accessed within processStream function call. Error at " ++ (showPos pos))]
evaluateMexpr defs vars (EOL pos) =  Null [("EOL can only be accessed within processStream function call. Error at " ++ (showPos pos))]                                        

evaluateMexpr defs vars (MathOp a Plus b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                    (case (x,y) of 
                                                          (IntegerValue one, IntegerValue two) -> IntegerValue (one+two)
                                                          (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of + function at " ++ showPos(pos)])
                                                          (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of + function at " ++ showPos(pos)]) )                                   

evaluateMexpr defs vars (MathOp a Minus b pos) =let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                    (case (x,y) of 
                                                          (IntegerValue one, IntegerValue two) -> IntegerValue (one-two)
                                                          (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of - function at " ++ showPos(pos)])
                                                          (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of - function at " ++ showPos(pos)]) )

evaluateMexpr defs vars (MathOp a Multiply b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                        (case (x,y) of 
                                                              (IntegerValue one, IntegerValue two) -> IntegerValue (one*two)
                                                              (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of * function at " ++ showPos(pos)])
                                                              (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of * function at " ++ showPos(pos)]) )

evaluateMexpr defs vars (MathOp a Divide b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                      (case (x,y) of 
                                                            (IntegerValue one, IntegerValue two)  -> IntegerValue (one `div` two)
                                                            (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of / function at " ++ showPos(pos)])
                                                            (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of / function at " ++ showPos(pos)]) )

evaluateMexpr defs vars (MathOp a Power b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                     (case (x,y) of 
                                                           (IntegerValue one, IntegerValue two) -> IntegerValue (one+two)
                                                           (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of ^ function at " ++ showPos(pos)])
                                                           (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of ^ function at " ++ showPos(pos)]) )
 


evaluateBexpr :: [Func_Def] -> [CallVariable] -> Bool_expr -> (Value)
evaluateBexpr defs vars (BoolVal val pos) = (BooleanValue val)
evaluateBexpr defs vars (MathToBool a Equal b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                         (case (x,y) of 
                                                               (IntegerValue one, IntegerValue two) -> BooleanValue (one==two)
                                                               (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of == function at " ++ showPos(pos)])
                                                               (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of == function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (MathToBool a Less b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                        (case (x,y) of 
                                                              (IntegerValue one, IntegerValue two) -> BooleanValue (one<two)
                                                              (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of < function at " ++ showPos(pos)])
                                                              (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of < function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (MathToBool a GreaterEqual b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                                (case (x,y) of 
                                                                      (IntegerValue one, IntegerValue two) -> BooleanValue (one>=two)
                                                                      (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of >= function at " ++ showPos(pos)])
                                                                      (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of >= function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (MathToBool a LessEqual b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                             (case (x,y) of  
                                                                   (IntegerValue one, IntegerValue two) -> BooleanValue (one<=two)
                                                                   (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of <= function at " ++ showPos(pos)])
                                                                   (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of <= function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (MathToBool a Greater b pos) =  let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                            (case (x,y) of 
                                                                  (IntegerValue one, IntegerValue two) -> BooleanValue (one >= two)
                                                                  (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of > function at " ++ showPos(pos)])
                                                                  (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of > function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (MathToBool a NotEqual b pos) = let (x,y) = (evaluateMathOptions defs vars a, evaluateMathOptions defs vars b) in
                                                            (case (x,y) of
                                                                  (IntegerValue one, IntegerValue two) -> BooleanValue (not (one==two))
                                                                  (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of != function at " ++ showPos(pos)])
                                                                  (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of != function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (BoolToBool a Or b pos) = let (x,y) = (evaluateBoolOptions defs vars a, evaluateBoolOptions defs vars b) in
                                                      (case (x,y) of 
                                                            (BooleanValue one, BooleanValue two) -> BooleanValue (one || two)
                                                            (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of || function at " ++ showPos(pos)])
                                                            (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of || function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (BoolToBool a And b pos) = let (x,y) = (evaluateBoolOptions defs vars a, evaluateBoolOptions defs vars b) in
                                                       (case (x,y) of  
                                                             (BooleanValue one, BooleanValue two) -> BooleanValue (one && two)
                                                             (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of && function at " ++ showPos(pos)])
                                                             (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of && function at " ++ showPos(pos)]) )

evaluateBexpr defs vars (Neg a pos) = let bool = (evaluateBoolOptions defs vars a) in
                                          (case bool of 
                                                (BooleanValue one) -> BooleanValue ( not one )
                                                (Null err) -> Null (err ++ ["Invalid value given to the ! function at " ++ showPos(pos)]) )


evaluateCexpr :: [Func_Def] -> [CallVariable] -> Cond_expr -> (Value)
evaluateCexpr defs vars cexpr = let cond = (evaluateBoolOptions defs vars (condExpr cexpr)) in
                                    (case cond of   
                                          (BooleanValue True) -> (evaluateExpr defs vars (trueExpr cexpr) )
                                          (BooleanValue False) -> (evaluateExpr defs vars (falseExpr cexpr) )
                                          (Null errors) -> Null (errors ++ [( "Invalid condition for if statement at  " ++ showPos(condExprPos(cexpr)) ) ]))



-- below function will check if each expr of the list has the same type. It will return the evaluated list after completion.
typeCheckList :: [Func_Def] -> [CallVariable] -> [Expr] -> [Value] -> AlexPosn -> Error [Value]

typeCheckList defs vars [] ys pos = return ys

typeCheckList defs vars (x:xs) ys pos | (length ys) == 0 = let a = (evaluateExpr defs vars x) in
                                                               (case (a) of
                                                                    (Null err) -> (Errors (err ++ ["Invalid element(s) within this list at " ++ showPos(pos) ]))
                                                                    (value) -> typeCheckList defs vars xs (ys ++ [value]) pos )
                                      | otherwise = let a = (evaluateExpr defs vars x) in
                                                        (case (a) of
                                                              (Null err) -> (Errors (err ++ ["Invalid element(s) within this list at " ++ showPos(pos) ]))
                                                              (value) -> ( if ( sameType value (head (reverse ys)) )
                                                                                    then typeCheckList defs vars xs (ys ++ [value]) pos
                                                                                    else (Errors [("Invalid element in list (doesn't match previous values in list). Error at " ++ (showPos pos))]) )  )
                                      
sameType :: Value -> Value -> Bool
sameType (IntegerValue _) (IntegerValue _) = True
sameType (BooleanValue _) (BooleanValue _) = True
sameType (ListValue (a:as) ) (ListValue (b:bs) ) = sameType a b
sameType (_) (_)  = False



evaluateLexpr :: [Func_Def] -> [CallVariable] ->  List_expr -> (Value)
evaluateLexpr defs vars (List list pos) = let ret = (typeCheckList defs vars list [] pos) in
                                              (case (ret) of 
                                                    (Valid list) -> (ListValue list)
                                                    (Errors err) ->  Null (err ++ ["Invalid list construction  " ++ showPos(pos)]) )
evaluateLexpr defs vars (Concat a b pos) = let (x,y) = (evaluateListOptions defs vars a, evaluateListOptions defs vars b) in
                                               (case (x,y) of
                                                     (ListValue one, ListValue two) -> ListValue (one ++ two)
                                                     (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of ++ function at " ++ showPos(pos)])
                                                     (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of ++ function at " ++ showPos(pos)]) )
evaluateLexpr defs vars (Tail a pos) = let list = (evaluateListOptions defs vars a) in
                                           (case list of 
                                                 (ListValue one) -> ListValue ( tail(one) )
                                                 (Null err) -> Null (err ++ ["Invalid value given to tail function at " ++ showPos(pos)]) )

evaluateLexpr defs vars (Row a pos) = Null ["You can't call row outside processStream. Error at: "++ (showPos pos)]
evaluateLexpr defs vars (Sequence a pos) = Null ["You can't call sequence outside processStream. Error at: "++ (showPos pos)]
evaluateLexpr defs vars (Streams pos) = Null ["You can't call stresam outside processStream. Error at: "++ (showPos pos)]

evaluateListFuncExpr :: [Func_Def] -> [CallVariable] ->  List_Func -> (Value)
evaluateListFuncExpr defs vars (Head list pos) =  let l = (evaluateListOptions defs vars list) in
                                                      (case l of 
                                                            (ListValue one) -> (head (one))
                                                            (Null err) -> Null (err ++ ["Invalid value given to head function at " ++ showPos(pos)]) )





evaluateListFuncExpr defs vars (ElemAt list math pos) = let (x,y) = (evaluateListOptions defs vars list, evaluateMathOptions defs vars math) in
                                                           (case (x,y) of 
                                                                 (ListValue ls, IntegerValue index)  -> (ls !! index)
                                                                 (Null err, _ ) -> Null (err ++ ["Invalid value given to first parameter of !! function at " ++ showPos(pos)])
                                                                 (_, Null err ) -> Null (err ++ ["Invalid value given to second parameter of !! function at " ++ showPos(pos)]) ) 
                                                       

getType :: Value -> String
getType (BooleanValue _) = "Bool"
getType (IntegerValue _) = "Int"
getType (ListValue _) = "List"




evaluateBoolOptions :: [Func_Def] -> [CallVariable] -> Bool_options ->  (Value)
evaluateBoolOptions defs vars (OptionBoolExpr expr) = (evaluateBexpr defs vars expr)
evaluateBoolOptions defs vars (BoolVar x pos) = evaluateExpr defs vars (Var x pos)
evaluateBoolOptions defs vars (BoolOut out) = evaluateOutStmt defs vars out           
evaluateBoolOptions defs vars (BoolProcessStream func) = evaluateProcessStreamStmt defs vars func
evaluateBoolOptions defs vars (BoolAccumalator pos) = evaluateExpr defs vars (Accumalator pos)
evaluateBoolOptions defs vars (BoolFunc func) = let val = (evaluateFuncStmt defs vars func) in
                                                    (case val of 
                                                          (BooleanValue bool) -> val
                                                          (Null errors) -> Null (errors ++ ["Problem occurred evaluating function at " ++ (showPos (callPos func)) ])
                                                          (_) -> Null ["This function doesn't evaluate to a boolean. Error at " ++ (showPos (callPos func)) ])


evaluateMathOptions :: [Func_Def] -> [CallVariable] -> Math_options -> (Value)
evaluateMathOptions defs vars (OptionMathExpr expr) = (evaluateMexpr defs vars expr)
evaluateMathOptions defs vars (MathVar x pos) = evaluateExpr defs vars (Var x pos) 
evaluateMathOptions defs vars (MathOut out) = evaluateOutStmt defs vars out           
evaluateMathOptions defs vars (MathProcessStream func) = evaluateProcessStreamStmt defs vars func
evaluateMathOptions defs vars (MathAccumalator pos) = evaluateExpr defs vars (Accumalator pos)
evaluateMathOptions defs vars (MathFunc func) = let val = (evaluateFuncStmt defs vars func) in
                                                    (case val of 
                                                          (IntegerValue int) -> val
                                                          (Null errors) -> Null (errors ++ ["Problem occurred evaluating function at " ++ (showPos (callPos func)) ])
                                                          (_) -> Null ["This function doesn't evaluate to an integer. Error at " ++ (showPos (callPos func)) ] )

evaluateListOptions :: [Func_Def] -> [CallVariable] -> List_options -> (Value)
evaluateListOptions defs vars (OptionListExpr expr) = (evaluateLexpr defs vars expr)
evaluateListOptions defs vars (ListVar x pos) = evaluateExpr defs vars (Var x pos)
evaluateListOptions defs vars (ListOut out) = evaluateOutStmt defs vars out           
evaluateListOptions defs vars (ListProcessStream func) = evaluateProcessStreamStmt defs vars func
evaluateListOptions defs vars (ListAccumalator pos) = evaluateExpr defs vars (Accumalator pos)
evaluateListOptions defs vars (ListFunc func) = let val = (evaluateFuncStmt defs vars func) in
                                                    (case val of 
                                                          (ListValue int) -> val
                                                          (Null errors) -> Null (errors ++ ["Problem occurred evaluating function at " ++ (showPos (callPos func)) ])
                                                          (_) -> Null ["This function doesn't evaluate to a list. Error at " ++ (showPos (callPos func)) ])













