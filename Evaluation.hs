
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


data ProcessStreamEnvironment = StreamEnvironment { accumalatorValue::Value, currentRow::[Int], stream::[[Int]], currentValue::Value} 
                              | EmptyPSE
                              deriving (Eq,Show)

 


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



evaluateProgram :: Program -> [IO Value]
evaluateProgram (Main lines) = let stmts = (getStatements lines)
                                   defs = (getFunctionDefinitions lines)
                               in [ (evaluateStmt defs [] EmptyPSE  x ) | x <- stmts]
             



evaluateStmt :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment ->  Stmt -> (IO Value)
evaluateStmt defs vars pSE (ProcessStreamStmt x) = evaluateProcessStreamStmt defs vars pSE x
evaluateStmt defs vars pSE (OutStmt x) = evaluateOutStmt defs vars pSE x
evaluateStmt defs vars pSE (CondStmt x) = evaluateCondStmt defs vars pSE x

getRow :: String -> [Int]
getRow (word) = map read $ words word :: [Int]


-- PLEASE FIX
ignoreLine count = do done <- isEOF
                      if (done || (count<=0) ) 
                         then return ()
                         else (do row <- getLine
                                  ignoreLine (count-1) )


handleProcessStream :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Int -> Int -> Int -> Process_Stream_Func -> (IO Value)
handleProcessStream defs vars pSE start end itr func   = do done <- isEOF
                                                            if (done || (start>=end) ) 
                                                               then (return (currentValue pSE))
                                                               else (do row <- getLine
                                                                        ignoreLine (itr-1) -- -> if you can get this function working, it would be great
                                                                        let rowInt = (getRow row)
                                                                        let strm = stream $ pSE
                                                                        let newStream = (strm ++ [rowInt] )
                                                                        let currentAcc =  accumalatorValue $ pSE
                                                                        let currentVal = currentValue $ pSE 
                                                                        newAcc <- evaluateExpr defs vars (StreamEnvironment currentAcc rowInt newStream (currentValue$pSE) )  (acc $ func)
                                                                        newVal <- evaluateExpr defs vars (StreamEnvironment currentAcc rowInt newStream (currentValue$pSE) )  (output $ func)
                                                                        (case (newAcc,newVal) of
                                                                             (Null err, _ ) -> return (Null (["Error occurred when applying accumalator function " ++ (showPos ( processStreamPos func)) ] ++ err ) )
                                                                             (_, Null err ) -> return (Null (["Error occurred when applying output functions " ++ (showPos (processStreamPos func)) ] ++ err ) )
                                                                             (acc,output) -> handleProcessStream defs vars (StreamEnvironment acc rowInt newStream output) (start+itr) end itr func) )


evaluateProcessStreamStmt :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Process_Stream_Func -> (IO Value) 
evaluateProcessStreamStmt defs vars pSE func = do initAcc <- evaluateExpr defs vars pSE (Grammar.init func)
                                                  strt <- evaluateMathOptions defs vars pSE (Grammar.start (Grammar.iterator (func)) )
                                                  end <- evaluateMathOptions defs vars pSE (Grammar.end (Grammar.iterator func))
                                                  incr <- evaluateMathOptions defs vars pSE (Grammar.incr (Grammar.iterator func))
                                                  case (initAcc,strt,end,incr) of
                                                       ((Null err),_,_,_) -> return (Null ( [ "Invalid initial accumalator value " ++ (showPos (processStreamPos func))  ] ++ err) )
                                                       (_,(Null err),_,_) -> return (Null ( [ "Invalid start value " ++ (showPos (processStreamPos func))  ] ++ err))
                                                       (_,_,(Null err),_) -> return (Null ( [ "Invalid end value " ++ (showPos (processStreamPos func))  ] ++ err))
                                                       (_,_,_,(Null err)) -> return (Null ( [ "Invalid increment value " ++ (showPos (processStreamPos func))  ] ++ err))
                                                       (initAcc,IntegerValue s,IntegerValue e,IntegerValue i) -> if (i >= 0 ) 
                                                                                                                then handleProcessStream defs vars (StreamEnvironment initAcc [] [] (Null ["No output function was applied."])) s e i func
                                                                                                                else return (Null ["ProcessStream increment can't be negative."])
printValue (BooleanValue value) = do putStr ( (show value) ++ " "  )
printValue (IntegerValue value) = do putStr ( (show value) ++ " "  )
printValue (ListValue []) = putStrLn ( "" )
printValue (ListValue (x:xs)) = do x <- (printValue x)
                                   printValue (ListValue xs)

printValue (Null (xs)) = do putStrLn "Errors "
                            printListOfStrings xs
printListOfStrings [] = putStr ("");
printListOfStrings (x:xs) = do putStrLn (x)
                               printListOfStrings(xs)

evaluateOutStmt :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment ->  Out_Func -> (IO Value)  
evaluateOutStmt defs vars pSE (Out (ProcessStreamExpr func) pos) = do x <- evaluateProcessStreamStmt defs vars pSE func
                                                                      printValue x
                                                                      return x
evaluateOutStmt defs vars pSE (Out expr pos) = do e <- evaluateExpr defs vars pSE expr
                                                  (case e of
                                                        (Null err) -> (do let v = Null (["Error occurred when evaluating expression at " ++ (showPos pos)] ++ err)
                                                                          printValue v
                                                                          return v ) 
                                                        (value) -> (do printValue value
                                                                       return (value) ) )


evaluateCondStmt :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Cond_stmt -> (IO Value) 
evaluateCondStmt defs vars pSE condStmt = do cond <- (evaluateBoolOptions defs vars pSE (stmtCond condStmt))
                                             (case cond of 
                                                   (BooleanValue True) -> (evaluateStmt defs vars pSE (trueStmt condStmt)  )
                                                   (BooleanValue False) -> (evaluateStmt defs vars pSE (falseStmt condStmt) )
                                                   (Null errors) -> return (Null ( [( "Invalid condition for if statement at  " ++ showPos(condStmtPos(condStmt)) ) ] ++ errors) ) )


getValueOfVariable :: [CallVariable] -> String -> AlexPosn -> Value
getValueOfVariable [] var pos = Null [("Invalid identifier at " ++ (showPos pos))]
getValueOfVariable (x:xs) var pos | (name x)==var = (value x)
                                  | otherwise = (getValueOfVariable xs var pos)



evaluateExpr :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Expr -> (IO Value)
evaluateExpr defs vars pSE (FuncStmt func) = evaluateFuncStmt defs vars pSE func
evaluateExpr defs vars pSE (MathExpr mexpr) = evaluateMexpr defs vars pSE mexpr
evaluateExpr defs vars pSE (CondExpr cexpr) = evaluateCexpr defs vars pSE cexpr
evaluateExpr defs vars pSE (BoolExpr bexpr) = evaluateBexpr defs vars pSE bexpr
evaluateExpr defs vars pSE (ListExpr lexpr) = evaluateLexpr defs vars pSE lexpr
evaluateExpr defs vars pSE (ListFuncExpr func) = evaluateListFuncExpr defs vars pSE func
evaluateExpr defs vars pSE (Var x pos) = return (getValueOfVariable vars x pos  )
evaluateExpr defs vars pSE (OutExpr outStmt) = evaluateOutStmt defs vars pSE outStmt
evaluateExpr defs vars pSE (ProcessStreamExpr func) = evaluateProcessStreamStmt defs vars pSE func
evaluateExpr defs vars pSE (StringExpr x pos) = return (Null [("Invalid identifier " ++ x ++ " at " ++ (showPos pos))])
evaluateExpr defs vars (StreamEnvironment acc _ _ _) (Accumalator pos) = return acc 
evaluateExpr defs vars (_) (Accumalator pos) = return (Null [("Accumalator can only be accessed within processStream function call. Error at " ++ (showPos pos))])
                       


getFuncDef :: [Func_Def] -> String -> AlexPosn -> (Error Func_Def)
getFuncDef [] callName pos =  Errors ["Function doesn't exist. Error at " ++(showPos pos) ]
getFuncDef (x:xs) callName pos | (defName x)==callName = return x
                               | otherwise = (getFuncDef xs callName pos)


data CallVariables = CallVariables {callVariables::[CallVariable]} | None [String] deriving (Eq,Show)

returnCallVariables :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Func_Def -> Param_list -> IO CallVariables

returnCallVariables defs vars pSE (Def n (ParamList [] ) _ _ ) (ParamList []) = return (CallVariables [])

returnCallVariables defs vars pSE (Def n (ParamList ((Var x _):xs)) e pos) (ParamList (paramExpr:ys)) = do val <- evaluateExpr defs vars pSE paramExpr
                                                                                                           (case val of 
                                                                                                                 (Null errors) -> return (None (["Error in given parameter at "++(showPos pos)] ++ errors))
                                                                                                                 (_) -> (do cvars <- (returnCallVariables defs vars pSE (Def n (ParamList xs) e pos) (ParamList ys)) 
                                                                                                                            return (CallVariables ( (Variable x val) : (callVariables cvars) )) )  )
                                                                                                       

returnCallVariables defs vars pSE (Def n (ParamList (_:xs)) e pos) (ParamList (y:ys)) = return (None ["Invalid function definition at " ++ (showPos pos)]    )



evaluateFuncStmt :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Func_stmt -> (IO Value)
evaluateFuncStmt defs vars pSE (Call name params pos) = let defErr = getFuncDef defs name pos in
                                                            (case defErr of 
                                                                  (Valid def)  -> (if (length (paramlist (defParams def)) == length (paramlist (params)))
                                                                                      then (do callVariables <- returnCallVariables defs vars pSE def params
                                                                                               (case callVariables of 
                                                                                                     (None errors) -> return (Null errors) 
                                                                                                     (CallVariables callVar) -> evaluateExpr defs callVar pSE (declr def) ) )
                                                                                      else return (Null ["An invalid number of parameters given to function call at " ++ (showPos pos)])  )
                                                                  (Errors errors) -> return (Null errors) )



evaluateMexpr :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Math_expr -> (IO Value)
evaluateMexpr defs vars pSE (IntVal int pos) = return (IntegerValue int)
evaluateMexpr defs vars pSE (Length list pos) = do lst <- evaluateListOptions defs vars pSE list
                                                   (case lst of 
                                                      (ListValue list) -> return (IntegerValue (length(list)) )
                                                      (Null err) -> return (Null (["Invalid list given to length function at "++ showPos(pos)] ++ err) ))

evaluateMexpr defs vars pSE (EOF pos) =  return (Null [("EOF can only be accessed within processStream function call. Error at " ++ (showPos pos))])
evaluateMexpr defs vars pSE (EOL pos) =  return (Null [("EOL can only be accessed within processStream function call. Error at " ++ (showPos pos))] )                                       

evaluateMexpr defs vars pSE (MathOp a Plus b pos) = do x <- (evaluateMathOptions defs vars pSE a)
                                                       y <- (evaluateMathOptions defs vars pSE b)
                                                       (case (x,y) of 
                                                          (IntegerValue one, IntegerValue two) -> return (IntegerValue (one+two))
                                                          (Null err, _ ) -> return (Null (["Invalid value given to first parameter of + function at " ++ showPos(pos)] ++ err))
                                                          (_, Null err ) -> return (Null (["Invalid value given to second parameter of + function at " ++ showPos(pos)] ++ err) ))                                   

evaluateMexpr defs vars pSE (MathOp a Minus b pos) =do  x <- (evaluateMathOptions defs vars pSE a)
                                                        y <- evaluateMathOptions defs vars pSE b
                                                        (case (x,y) of 
                                                              (IntegerValue one, IntegerValue two) -> return (IntegerValue (one-two))
                                                              (Null err, _ ) -> return (Null (["Invalid value given to first parameter of - function at " ++ showPos(pos)] ++ err))
                                                              (_, Null err ) -> return (Null (["Invalid value given to second parameter of - function at " ++ showPos(pos)] ++ err) ))

evaluateMexpr defs vars pSE (MathOp a Multiply b pos) = do x <- (evaluateMathOptions defs vars pSE a)
                                                           y <- evaluateMathOptions defs vars pSE b
                                                           (case (x,y) of 
                                                                  (IntegerValue one, IntegerValue two) -> return (IntegerValue (one*two))
                                                                  (Null err, _ ) -> return (Null (["Invalid value given to first parameter of * function at " ++ showPos(pos)] ++ err))
                                                                  (_, Null err ) -> return (Null (["Invalid value given to second parameter of * function at " ++ showPos(pos)] ++ err) ))

evaluateMexpr defs vars pSE (MathOp a Divide b pos) = do x <- (evaluateMathOptions defs vars pSE a)
                                                         y <- evaluateMathOptions defs vars pSE b
                                                         (case (x,y) of 
                                                            (IntegerValue one, IntegerValue two)  -> return (IntegerValue (one `div` two))
                                                            (Null err, _ ) -> return (Null (["Invalid value given to first parameter of / function at " ++ showPos(pos)] ++ err))
                                                            (_, Null err ) -> return (Null (["Invalid value given to second parameter of / function at " ++ showPos(pos)] ++ err) ))

evaluateMexpr defs vars pSE (MathOp a Power b pos) = do  x <- (evaluateMathOptions defs vars pSE a)
                                                         y <- evaluateMathOptions defs vars pSE b
                                                         (case (x,y) of 
                                                           (IntegerValue one, IntegerValue two) -> return (IntegerValue (one+two))
                                                           (Null err, _ ) -> return (Null (["Invalid value given to first parameter of ^ function at " ++ showPos(pos)] ++ err))
                                                           (_, Null err ) -> return (Null (["Invalid value given to second parameter of ^ function at " ++ showPos(pos)] ++ err) ))
 


evaluateBexpr :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Bool_expr -> (IO Value)
evaluateBexpr defs vars pSE (BoolVal val pos) = return (BooleanValue val)
evaluateBexpr defs vars pSE (MathToBool a Equal b pos) = do x <- (evaluateMathOptions defs vars pSE a)
                                                            y <- evaluateMathOptions defs vars pSE b
                                                            (case (x,y) of 
                                                               (IntegerValue one, IntegerValue two) -> return (BooleanValue (one==two))
                                                               (Null err, _ ) -> return (Null (["Invalid value given to first parameter of == function at " ++ showPos(pos)] ++ err))
                                                               (_, Null err ) -> return (Null (["Invalid value given to second parameter of == function at " ++ showPos(pos)] ++ err) ))


evaluateBexpr defs vars pSE (MathToBool a Less b pos) = do x <- evaluateMathOptions defs vars pSE a
                                                           y <- evaluateMathOptions defs vars pSE b
                                                           (case (x,y) of 
                                                              (IntegerValue one, IntegerValue two) -> return (BooleanValue (one<two))
                                                              (Null err, _ ) -> return (Null (["Invalid value given to first parameter of < function at " ++ showPos(pos)] ++ err))
                                                              (_, Null err ) -> return (Null (["Invalid value given to second parameter of < function at " ++ showPos(pos)] ++ err) ) )


evaluateBexpr defs vars pSE (MathToBool a GreaterEqual b pos) = do x <- (evaluateMathOptions defs vars pSE a)
                                                                   y <- evaluateMathOptions defs vars pSE b
                                                                   (case (x,y) of 
                                                                      (IntegerValue one, IntegerValue two) -> return (BooleanValue (one>=two))
                                                                      (Null err, _ ) -> return (Null (["Invalid value given to first parameter of >= function at " ++ showPos(pos)] ++ err))
                                                                      (_, Null err ) -> return (Null (["Invalid value given to second parameter of >= function at " ++ showPos(pos)] ++ err) ))

evaluateBexpr defs vars pSE (MathToBool a LessEqual b pos) = do  x <- (evaluateMathOptions defs vars pSE a)
                                                                 y <- evaluateMathOptions defs vars pSE b 
                                                                 (case (x,y) of  
                                                                   (IntegerValue one, IntegerValue two) -> return (BooleanValue (one<=two))
                                                                   (Null err, _ ) -> return (Null (["Invalid value given to first parameter of <= function at " ++ showPos(pos)] ++ err))
                                                                   (_, Null err ) -> return (Null (["Invalid value given to second parameter of <= function at " ++ showPos(pos)] ++ err) ))

evaluateBexpr defs vars pSE (MathToBool a Greater b pos) =  do x <- (evaluateMathOptions defs vars pSE a)
                                                               y <- evaluateMathOptions defs vars pSE b 
                                                               (case (x,y) of 
                                                                  (IntegerValue one, IntegerValue two) -> return (BooleanValue (one >= two))
                                                                  (Null err, _ ) -> return (Null (["Invalid value given to first parameter of > function at " ++ showPos(pos)] ++ err))
                                                                  (_, Null err ) -> return (Null (["Invalid value given to second parameter of > function at " ++ showPos(pos)] ++ err) ))

evaluateBexpr defs vars pSE (MathToBool a NotEqual b pos) = do x <- (evaluateMathOptions defs vars pSE a)
                                                               y <- evaluateMathOptions defs vars pSE b 
                                                               (case (x,y) of
                                                                  (IntegerValue one, IntegerValue two) -> return (BooleanValue (not (one==two)))
                                                                  (Null err, _ ) -> return (Null (["Invalid value given to first parameter of != function at " ++ showPos(pos)] ++ err))
                                                                  (_, Null err ) -> return (Null (["Invalid value given to second parameter of != function at " ++ showPos(pos)] ++ err) ))

evaluateBexpr defs vars pSE (BoolToBool a Or b pos) = do x <- (evaluateBoolOptions defs vars pSE a)
                                                         y <- (evaluateBoolOptions defs vars pSE b) 
                                                         (case (x,y) of 
                                                            (BooleanValue one, BooleanValue two) -> return (BooleanValue (one || two))
                                                            (Null err, _ ) -> return (Null (["Invalid value given to first parameter of || function at " ++ showPos(pos)] ++ err))
                                                            (_, Null err ) -> return (Null (["Invalid value given to second parameter of || function at " ++ showPos(pos)] ++ err) ))

evaluateBexpr defs vars pSE (BoolToBool a And b pos) = do x <- (evaluateBoolOptions defs vars pSE a)
                                                          y <- (evaluateBoolOptions defs vars pSE b) 
                                                          (case (x,y) of  
                                                             (BooleanValue one, BooleanValue two) -> return (BooleanValue (one && two))
                                                             (Null err, _ ) -> return (Null (["Invalid value given to first parameter of && function at " ++ showPos(pos)] ++ err))
                                                             (_, Null err ) -> return( Null (["Invalid value given to second parameter of && function at " ++ showPos(pos)] ++ err) ))

evaluateBexpr defs vars pSE (Neg a pos) = do bool <- (evaluateBoolOptions defs vars pSE a)
                                             (case bool of 
                                                (BooleanValue one) -> return (BooleanValue ( not one ))
                                                (Null err) -> return (Null (["Invalid value given to the ! function at " ++ showPos(pos)] ++ err) ))


evaluateCexpr :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Cond_expr -> (IO Value)
evaluateCexpr defs vars pSE cexpr = do cond <- (evaluateBoolOptions defs vars pSE (condExpr cexpr))
                                       (case cond of   
                                          (BooleanValue True) -> (evaluateExpr defs vars pSE (trueExpr cexpr) )
                                          (BooleanValue False) -> (evaluateExpr defs vars pSE (falseExpr cexpr) )
                                          (Null errors) -> return (Null ([( "Invalid condition for if statement at  " ++ showPos(condExprPos(cexpr)) ) ] ++ errors)))



-- below function will check if each expr of the list has the same type. It will return the evaluated list after completion.
typeCheckList :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> [Expr] -> [Value] -> AlexPosn -> IO Value

typeCheckList defs vars pSE [] ys pos = return (ListValue ys)

typeCheckList defs vars pSE (x:xs) ys pos | (length ys) == 0 = do a <- (evaluateExpr defs vars pSE x)
                                                                  (case (a) of
                                                                    (Null err) -> return (Null (["Invalid element(s) within this list at " ++ showPos(pos) ] ++ err))
                                                                    (value) -> (typeCheckList defs vars pSE xs (ys ++ [value]) pos ))
                                          | otherwise = do a <- (evaluateExpr defs vars pSE x)
                                                           (case (a) of
                                                              (Null err) -> return (Null (["Invalid element(s) within this list at " ++ showPos(pos) ]++ err ))
                                                              (value) -> ( if ( sameType value (head (reverse ys)) )
                                                                              then typeCheckList defs vars pSE xs (ys ++ [value]) pos
                                                                              else return (Null [("Invalid element in list (doesn't match previous values in list). Error at " ++ (showPos pos))]) )  )
                                      
sameType :: Value -> Value -> Bool
sameType (IntegerValue _) (IntegerValue _) = True
sameType (BooleanValue _) (BooleanValue _) = True
sameType (ListValue (a:as) ) (ListValue (b:bs) ) = sameType a b
sameType (_) (_)  = False



evaluateLexpr :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment ->  List_expr -> (IO Value)
evaluateLexpr defs vars pSE (List list pos) = do ret <- (typeCheckList defs vars pSE list [] pos)
                                                 (case (ret) of 
                                                    (ListValue l ) -> return (ListValue l )
                                                    (Null err) ->  return (Null (["Invalid list construction  " ++ showPos(pos)] ++ err) ))
evaluateLexpr defs vars pSE (Concat a b pos) = do x <- (evaluateListOptions defs vars pSE a)
                                                  y <- evaluateListOptions defs vars pSE b 
                                                  (case (x,y) of
                                                     (ListValue one, ListValue two) -> return (ListValue (one ++ two))
                                                     (Null err, _ ) -> return (Null (["Invalid value given to first parameter of ++ function at " ++ showPos(pos)] ++ err))
                                                     (_, Null err ) -> return (Null (["Invalid value given to second parameter of ++ function at " ++ showPos(pos)] ++ err) ))

evaluateLexpr defs vars pSE (Tail a pos) = do list <- (evaluateListOptions defs vars pSE a)
                                              (case list of 
                                                 (ListValue one) -> return (ListValue ( tail(one) ))
                                                 (Null err) -> return (Null (["Invalid value given to tail function at " ++ showPos(pos)] ++ err) ))

evaluateLexpr defs vars (StreamEnvironment x row y z) (Row pos) = return (ListValue (fromIntList row))

evaluateLexpr defs vars (_) (Row pos) =  return (Null ["You can't call row outside processStream. Error at: "++ (showPos pos)])

evaluateLexpr defs vars pSE (Sequence a pos) = return (Null ["You can't call sequence outside processStream. Error at: "++ (showPos pos)])

evaluateLexpr defs vars (StreamEnvironment _ _ stream _) (Streams pos) = return (ListValue (fromStream stream))

evaluateLexpr defs vars pSE (Streams pos) = return (Null ["You can't call stresam outside processStream. Error at: "++ (showPos pos)])

fromIntList :: [Int] -> [Value]
fromIntList [] = []
fromIntList (x:xs) = (IntegerValue x) : fromIntList xs

fromStream :: [[Int]] -> [Value]
fromStream [] = [] 
fromStream (xs:xss) = (ListValue (fromIntList xs)) : fromStream xss


--data ProcessStreamEnvironment = StreamEnvironment { accumalatorValue::Value, currentRow::[Int], stream::[[Int]], currentValue::Value} 
    


evaluateListFuncExpr :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment ->  List_Func -> (IO Value)
evaluateListFuncExpr defs vars pSE (Head list pos) = do l <- (evaluateListOptions defs vars pSE list)
                                                        (case l of 
                                                           (ListValue one) -> return (head (one))
                                                           (Null err) -> return (Null (["Invalid value given to head function at " ++ showPos(pos)] ++ err) ))





evaluateListFuncExpr defs vars pSE (ElemAt list math pos) = do x <- (evaluateListOptions defs vars pSE list)
                                                               y <- evaluateMathOptions defs vars pSE math 
                                                               (case (x,y) of 
                                                                 (ListValue ls, IntegerValue index)  -> return (ls !! index)
                                                                 (Null err, _ ) -> return (Null (["Invalid value given to first parameter of !! function at " ++ showPos(pos)] ++ err))
                                                                 (_, Null err ) -> return (Null (["Invalid value given to second parameter of !! function at " ++ showPos(pos)] ++ err) ) )
                                                       

getType :: Value -> String
getType (BooleanValue _) = "Bool"
getType (IntegerValue _) = "Int"
getType (ListValue _) = "List"




evaluateBoolOptions :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Bool_options ->  (IO Value)
evaluateBoolOptions defs vars pSE (OptionBoolExpr expr) = (evaluateBexpr defs vars pSE expr)
evaluateBoolOptions defs vars pSE (BoolVar x pos) = evaluateExpr defs vars pSE (Var x pos)
evaluateBoolOptions defs vars pSE (BoolOut out) = evaluateOutStmt defs vars pSE out           
evaluateBoolOptions defs vars pSE (BoolProcessStream func) = evaluateProcessStreamStmt defs vars pSE func
evaluateBoolOptions defs vars pSE (BoolAccumalator pos) = evaluateExpr defs vars pSE (Accumalator pos)
evaluateBoolOptions defs vars pSE (BoolFunc func) = do val <- (evaluateFuncStmt defs vars pSE func)
                                                       (case val of 
                                                          (BooleanValue bool) -> return val
                                                          (Null errors) -> return (Null (["Problem occurred evaluating function at " ++ (showPos (callPos func)) ] ++ errors) )
                                                          (_) -> return (Null ["This function doesn't evaluate to a boolean. Error at " ++ (showPos (callPos func)) ]))


evaluateMathOptions :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> Math_options -> (IO Value)
evaluateMathOptions defs vars pSE (OptionMathExpr expr) = (evaluateMexpr defs vars pSE expr)
evaluateMathOptions defs vars pSE (MathVar x pos) = evaluateExpr defs vars pSE (Var x pos) 
evaluateMathOptions defs vars pSE (MathOut out) = evaluateOutStmt defs vars pSE out           
evaluateMathOptions defs vars pSE (MathProcessStream func) = evaluateProcessStreamStmt defs vars pSE func
evaluateMathOptions defs vars pSE (MathAccumalator pos) = evaluateExpr defs vars pSE (Accumalator pos)
evaluateMathOptions defs vars pSE (MathFunc func) = do val <- (evaluateFuncStmt defs vars pSE func)
                                                       (case val of 
                                                          (IntegerValue int) -> return val
                                                          (Null errors) ->  return (Null (["Problem occurred evaluating function at " ++ (showPos (callPos func)) ] ++ errors))
                                                          (_) -> return (Null ["This function doesn't evaluate to an integer. Error at " ++ (showPos (callPos func)) ] ))

evaluateListOptions :: [Func_Def] -> [CallVariable] -> ProcessStreamEnvironment -> List_options -> (IO Value)
evaluateListOptions defs vars pSE (OptionListExpr expr) = (evaluateLexpr defs vars pSE expr)
evaluateListOptions defs vars pSE (ListVar x pos) = evaluateExpr defs vars pSE (Var x pos)
evaluateListOptions defs vars pSE (ListOut out) = evaluateOutStmt defs vars pSE out           
evaluateListOptions defs vars pSE (ListProcessStream func) =  evaluateProcessStreamStmt defs vars pSE func
evaluateListOptions defs vars pSE (ListAccumalator pos) = evaluateExpr defs vars pSE (Accumalator pos)
evaluateListOptions defs vars pSE (ListFunc func) = do val <- (evaluateFuncStmt defs vars pSE func)
                                                       (case val of 
                                                          (ListValue int) -> return val
                                                          (Null errors) -> return (Null (["Problem occurred evaluating function at " ++ (showPos (callPos func)) ] ++ errors))
                                                          (_) -> return (Null ["This function doesn't evaluate to a list. Error at " ++ (showPos (callPos func)) ]))













