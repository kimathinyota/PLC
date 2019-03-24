
module Evaluation where 
import Tokens
import Grammar

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
           deriving (Eq,Show)


data CallVariable = Variable {name::String, value::Value} deriving (Eq,Show)

evaluateProgram :: Program -> [Error Value]
evaluateProgram (Main lines) = let stmts = (getStatements lines)
                                   defs = (getFunctionDefinitions lines)
                               in [ (evaluateStmt defs [] x) | x <- stmts]
             



evaluateStmt :: [Func_Def] -> [CallVariable] ->  Stmt -> (Error Value)
evaluateStmt defs vars (ProcessStreamStmt x) = evaluateProcessStreamStmt defs vars x
evaluateStmt defs vars (OutStmt x) = evaluateOutStmt defs vars x
evaluateStmt defs vars (CondStmt x) = evaluateCondStmt defs vars x



evaluateProcessStreamStmt :: [Func_Def] -> [CallVariable] -> Process_Stream_Func -> (Error Value) 
evaluateProcessStreamStmt defs vars processStreamFunc = Errors [""]

evaluateOutStmt :: [Func_Def] -> [CallVariable] -> Out_Func -> (Error Value) 
evaluateOutStmt defs vars (Out expr _) = evaluateExpr defs vars expr

evaluateCondStmt :: [Func_Def] -> [CallVariable] -> Cond_stmt -> (Error Value) 
evaluateCondStmt defs vars condStmt = do cond <- (evaluateBoolOptions defs vars (stmtCond condStmt))
                                         if ( (valBool cond) == True )
                                            then (evaluateStmt defs vars (trueStmt condStmt) )
                                            else (evaluateStmt defs vars (falseStmt condStmt) )


getValueOfVariable :: [CallVariable] -> String -> AlexPosn -> Error Value
getValueOfVariable [] var pos = Errors [("Invalid identifier at " ++ (showPos pos))]
getValueOfVariable (x:xs) var pos | (name x)==var = return (value x)
                                  | otherwise = (getValueOfVariable xs var pos)



evaluateExpr :: [Func_Def] -> [CallVariable] -> Expr -> (Error Value)
evaluateExpr defs vars (FuncStmt func) = evaluateFuncStmt defs vars func
evaluateExpr defs vars (MathExpr mexpr) = evaluateMexpr defs vars mexpr
evaluateExpr defs vars (CondExpr cexpr) = evaluateCexpr defs vars cexpr
evaluateExpr defs vars (BoolExpr bexpr) = evaluateBexpr defs vars bexpr
evaluateExpr defs vars (ListExpr lexpr) = evaluateLexpr defs vars lexpr
evaluateExpr defs vars (ListFuncExpr func) = evaluateListFuncExpr defs vars func
evaluateExpr defs vars (Var x pos) = getValueOfVariable vars x pos  
evaluateExpr defs vars (OutExpr outStmt) = (evaluateOutStmt defs vars outStmt)
evaluateExpr defs vars (ProcessStreamExpr func) = evaluateProcessStreamStmt defs vars func
evaluateExpr defs vars (StringExpr x pos) = Errors [("Invalid identifier " ++ x ++ " at " ++ (showPos pos))]
evaluateExpr defs vars (Accumalator pos) = Errors [("Accumalator can only be accessed within processStream function call. Error at " ++ (showPos pos))]




getFuncDef :: [Func_Def] -> String -> AlexPosn -> (Error Func_Def)




getFuncDef [] callName pos = Errors ["Function doesn't exist. Error at " ++(showPos pos) ]
getFuncDef (x:xs) callName pos | (defName x)==callName = return x
                               | otherwise = (getFuncDef xs callName pos)



returnCallVariables :: [Func_Def] -> [CallVariable] -> Func_Def -> Param_list -> Error [CallVariable]

returnCallVariables defs vars (Def n (ParamList [] ) _ _ ) (ParamList []) = return []

returnCallVariables defs vars (Def n (ParamList ((Var x _):xs)) e pos) (ParamList (paramExpr:ys)) = do val <- evaluateExpr defs vars paramExpr
                                                                                                       cvars <- (returnCallVariables defs vars (Def n (ParamList xs) e pos) (ParamList ys))
                                                                                                       return ( (Variable x val) : cvars )

returnCallVariables defs vars (Def n (ParamList (_:xs)) e pos) (ParamList (y:ys)) = Errors["Invalid function definition at " ++ (showPos pos)]    



evaluateFuncStmt :: [Func_Def] -> [CallVariable] -> Func_stmt -> (Error Value)
evaluateFuncStmt defs vars (Call name params pos) = do def <- getFuncDef defs name pos
                                                       if (length (paramlist (defParams def)) == length (paramlist (params)))
                                                          then (do callVars <- returnCallVariables defs vars def params
                                                                   evaluateExpr defs callVars (declr def) ) 
                                                          else Errors["An invalid number of parameters given to function call at " ++ (showPos pos)]

                                                  
 


evaluateMexpr :: [Func_Def] -> [CallVariable] -> Math_expr -> (Error Value)
evaluateMexpr defs vars (IntVal int pos) = return (IntegerValue int)
evaluateMexpr defs vars (Length list pos) = do x <- evaluateListOptions defs vars list
                                               return (IntegerValue ( length (listVal x) ))
evaluateMexpr defs vars (EOF pos) =  Errors [("EOF can only be accessed within processStream function call. Error at " ++ (showPos pos))]
evaluateMexpr defs vars (EOL pos) =  Errors [("EOL can only be accessed within processStream function call. Error at " ++ (showPos pos))]                                        

evaluateMexpr defs vars (MathOp a Plus b pos) = do x <- evaluateMathOptions defs vars a
                                                   y <- evaluateMathOptions defs vars b
                                                   return (IntegerValue ((valInt x) + (valInt y) ))                                          

evaluateMexpr defs vars (MathOp a Minus b pos) = do x <- evaluateMathOptions defs vars a
                                                    y <- evaluateMathOptions defs vars b
                                                    return (IntegerValue ((valInt x) - (valInt y) ) )

evaluateMexpr defs vars (MathOp a Multiply b pos) = do x <- evaluateMathOptions defs vars a
                                                       y <- evaluateMathOptions defs vars b
                                                       return (IntegerValue ((valInt x) * (valInt y) ) )

evaluateMexpr defs vars (MathOp a Divide b pos) = do x <- evaluateMathOptions defs vars a
                                                     y <- evaluateMathOptions defs vars b
                                                     return (IntegerValue ((valInt x) `div` (valInt y) ) )

evaluateMexpr defs vars (MathOp a Power b pos) = do x <- evaluateMathOptions defs vars a
                                                    y <- evaluateMathOptions defs vars b
                                                    return (IntegerValue ((valInt x) ^ (valInt y) ) )
 


evaluateBexpr :: [Func_Def] -> [CallVariable] -> Bool_expr -> (Error Value)
evaluateBexpr defs vars (BoolVal val pos) = return (BooleanValue val)
evaluateBexpr defs vars (MathToBool a Equal b pos) = do x <- evaluateMathOptions defs vars a
                                                        y <- evaluateMathOptions defs vars b
                                                        return (BooleanValue ( (valInt x) == (valInt y) ))

evaluateBexpr defs vars (MathToBool a Less b pos) = do x <- evaluateMathOptions defs vars a
                                                       y <- evaluateMathOptions defs vars b
                                                       return (BooleanValue ( (valInt x) < (valInt y) ))

evaluateBexpr defs vars (MathToBool a GreaterEqual b pos) = do x <- evaluateMathOptions defs vars a
                                                               y <- evaluateMathOptions defs vars b
                                                               return (BooleanValue ( (valInt x) >= (valInt y) ))

evaluateBexpr defs vars (MathToBool a LessEqual b pos) = do x <- evaluateMathOptions defs vars a
                                                            y <- evaluateMathOptions defs vars b
                                                            return (BooleanValue ( (valInt x) <= (valInt y) ) )

evaluateBexpr defs vars (MathToBool a Greater b pos) = do x <- evaluateMathOptions defs vars a
                                                          y <- evaluateMathOptions defs vars b
                                                          return  (BooleanValue ( (valInt x) > (valInt y) ) )

evaluateBexpr defs vars (MathToBool a NotEqual b pos) = do x <- evaluateMathOptions defs vars a
                                                           y <- evaluateMathOptions defs vars b
                                                           return (BooleanValue ( not ((valInt x) == (valInt y)) ))

evaluateBexpr defs vars (BoolToBool a Or b pos) = do x <- evaluateBoolOptions defs vars a
                                                     y <- evaluateBoolOptions defs vars b
                                                     return (BooleanValue ( (valBool x) || (valBool y) ) )

evaluateBexpr defs vars (BoolToBool a And b pos) = do x <- evaluateBoolOptions defs vars a
                                                      y <- evaluateBoolOptions defs vars b
                                                      return (BooleanValue ( (valBool x) && (valBool y) ) )

evaluateBexpr defs vars (Neg a pos) = do x <- evaluateBoolOptions defs vars a
                                         return (BooleanValue ( not (valBool x) ) )

evaluateCexpr :: [Func_Def] -> [CallVariable] -> Cond_expr -> (Error Value)
evaluateCexpr defs vars cexpr = do cond <- (evaluateBoolOptions defs vars (condExpr cexpr))
                                   if ( (valBool cond) == True )
                                      then (evaluateExpr defs vars (trueExpr cexpr) )
                                      else (evaluateExpr defs vars (falseExpr cexpr) )


-- below function will check if each expr of the list has the same type. It will return the evaluated list after completion.
typeCheckList :: [Func_Def] -> [CallVariable] -> [Expr] -> [Value] -> AlexPosn -> Error [Value]

typeCheckList defs vars [] ys pos = return ys

typeCheckList defs vars (x:xs) ys pos | (length ys) == 0 = do a <- evaluateExpr defs vars x
                                                              typeCheckList defs vars xs (ys ++ [a]) pos
                                      | otherwise = do a <- evaluateExpr defs vars x
                                                       cond <- sameType a (head (reverse ys))
                                                       if ( cond==True )
                                                          then typeCheckList defs vars xs (ys ++ [a]) pos
                                                          else Errors [("Invalid element in list (doesn't match previous values in list). Error at " ++ (showPos pos))]


sameType :: Value -> Value -> Error Bool
sameType (IntegerValue _) (IntegerValue _) = return True
sameType (BooleanValue _) (BooleanValue _) = return True
sameType (ListValue (a:as) ) (ListValue (b:bs) ) = sameType a b
sameType (_) (_)  = return False



evaluateLexpr :: [Func_Def] -> [CallVariable] ->  List_expr -> (Error Value)
evaluateLexpr defs vars (List list pos) = do ret <- typeCheckList defs vars list [] pos
                                             return (ListValue ret)
evaluateLexpr defs vars (Concat a b pos) = do x <- evaluateListOptions defs vars a
                                              y <- evaluateListOptions defs vars b
                                              return (ListValue ( (listVal x) ++ (listVal y)))
evaluateLexpr defs vars (Tail a pos) = do x <- evaluateListOptions defs vars a
                                          return (ListValue (tail (listVal x)))

evaluateLexpr defs vars (Row a pos) = Errors ["You can't call row outside processStream. Error at: "++ (showPos pos)]
evaluateLexpr defs vars (Sequence a pos) = Errors ["You can't call sequence outside processStream. Error at: "++ (showPos pos)]
evaluateLexpr defs vars (Streams pos) = Errors ["You can't call stresam outside processStream. Error at: "++ (showPos pos)]

evaluateListFuncExpr :: [Func_Def] -> [CallVariable] ->  List_Func -> (Error Value)
evaluateListFuncExpr defs vars (Head list pos) = do x <- (evaluateListOptions defs vars list)
                                                    return (head(listVal x))


evaluateListFuncExpr defs vars (ElemAt list math pos) = do x <- (evaluateListOptions defs vars list)
                                                           y <- (evaluateMathOptions defs vars math)
                                                           return ((listVal x) !! (valInt y))



getType :: Value -> String
getType (BooleanValue _) = "Bool"
getType (IntegerValue _) = "Int"
getType (ListValue _) = "List"



evaluateBoolOptions :: [Func_Def] -> [CallVariable] -> Bool_options -> (Error Value)
evaluateBoolOptions defs vars (OptionBoolExpr expr) = (evaluateBexpr defs vars expr)
evaluateBoolOptions defs vars (BoolVar x pos) = evaluateExpr defs vars (Var x pos)
evaluateBoolOptions defs vars (BoolOut out) = evaluateOutStmt defs vars out           
evaluateBoolOptions defs vars (BoolProcessStream func) = evaluateProcessStreamStmt defs vars func
evaluateBoolOptions defs vars (BoolAccumalator pos) = evaluateExpr defs vars (Accumalator pos)
evaluateBoolOptions defs vars (BoolFunc func) = do val <- evaluateFuncStmt defs vars func
                                                   if ((getType val)=="Bool")
                                                      then (return val)
                                                      else (Errors ["This function doesn't evaluate to a boolean. Error at " ++ (showPos (callPos func)) ])

evaluateMathOptions :: [Func_Def] -> [CallVariable] -> Math_options -> (Error Value)
evaluateMathOptions defs vars (OptionMathExpr expr) = (evaluateMexpr defs vars expr)
evaluateMathOptions defs vars (MathVar x pos) = evaluateExpr defs vars (Var x pos) 
evaluateMathOptions defs vars (MathOut out) = evaluateOutStmt defs vars out           
evaluateMathOptions defs vars (MathProcessStream func) = evaluateProcessStreamStmt defs vars func
evaluateMathOptions defs vars (MathAccumalator pos) = evaluateExpr defs vars (Accumalator pos)
evaluateMathOptions defs vars (MathFunc func) = do val <- evaluateFuncStmt defs vars func
                                                   if ((getType val)=="Int")
                                                      then (return val)
                                                      else (Errors ["This function doesn't evaluate to an integer. Error at " ++ (showPos (callPos func)) ])

evaluateListOptions :: [Func_Def] -> [CallVariable] -> List_options -> (Error Value)
evaluateListOptions defs vars (OptionListExpr expr) = (evaluateLexpr defs vars expr)
evaluateListOptions defs vars (ListVar x pos) = evaluateExpr defs vars (Var x pos)
evaluateListOptions defs vars (ListOut out) = evaluateOutStmt defs vars out           
evaluateListOptions defs vars (ListProcessStream func) = evaluateProcessStreamStmt defs vars func
evaluateListOptions defs vars (ListAccumalator pos) = evaluateExpr defs vars (Accumalator pos)
evaluateListOptions defs vars (ListFunc func) = do val <- evaluateFuncStmt defs vars func
                                                   if ((getType val)=="List")
                                                      then (return val)
                                                      else (Errors ["This function doesn't evaluate to a list. Error at " ++ (showPos (callPos func)) ])













