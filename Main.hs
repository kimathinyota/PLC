import Tokens
import Grammar
import Evaluation
import System.Environment
import Control.Exception
import System.IO


main :: IO ()
main = cool2

tokens' fileName  = do sourceText <- readFile (fileName)
                       return ((alexScanTokens sourceText))

printAll ([]) = return ()
printAll (x:xs) = do l <- x
                     printAll(xs)

cool' fileName = do sourceText <- readFile (fileName)
                    --putStrLn ("Parsing : " ++ sourceText)
                    let parsedProg = parseCalc (alexScanTokens sourceText)
                    --putStrLn ("Parsed as " ++ (show parsedProg) )
                    printAll(evaluateProgram parsedProg)

test' fileName = do sourceText <- readFile (fileName)
                    putStrLn ("Parsing : " ++ sourceText)
                    let parsedProg = parseCalc (alexScanTokens sourceText)
                    putStrLn ("Parsed as " ++ (show parsedProg) )


cool2          = do (fileName : _ ) <- getArgs 
                    sourceText <- readFile (fileName)
                    --putStrLn ("Parsing : " ++ sourceText)
                    let parsedProg = parseCalc (alexScanTokens sourceText)
                    --putStrLn ("Parsed as " ++ (show parsedProg) )
                    printAll(evaluateProgram parsedProg)

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg))

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()