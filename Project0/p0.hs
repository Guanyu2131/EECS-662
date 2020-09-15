{-# LANGUAGE GADTs, FlexibleContexts #-}
-- Author: Guanyu
-- FileName: p0.hs
-- Project Description: The objective of this miniproject is to develop the first rudimentary interpreter.
-- This program will extend with the AE language presented in class to get up-to-speed with Haskell and write three 
-- different interpreters using three different styles.
-- Date: 9/14/2020


-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)
                     

term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.


-- Exercise 1
evalAE :: AE -> Int
evalAE (Num n) = if n < 0 then error "Num < 0" else n

evalAE (Plus l r) = (evalAE l) + (evalAE r)

evalAE (Minus l r) = if (evalAE l) < (evalAE r) then error "l - r < 0" else (evalAE l - evalAE r)

evalAE (Mult l r) = (evalAE l) * (evalAE r)

evalAE (Div l r) = if (evalAE r) == 0 then error "beichushu = 0" else ((evalAE l) `div` (evalAE r))

evalAE (If0 c t e) = if (evalAE c) == 0 then (evalAE t) else (evalAE e)


-- Exercise 2
evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num n) = if n < 0 then Nothing else Just n

evalAEMaybe (Plus l r) = case (evalAEMaybe l) of
  Just l' -> case (evalAEMaybe r) of
    Just r' -> Just (l' + r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (Minus l r) = case (evalAEMaybe l) of
  Just l' -> case (evalAEMaybe r) of
    Just r' -> if (l' < r') then Nothing else Just (l' - r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (Mult l r) = case (evalAEMaybe l) of
  Just l' -> case (evalAEMaybe r) of
    Just r' -> Just (l' * r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (Div l r) = case (evalAEMaybe l) of
  Just l' -> case (evalAEMaybe r) of
    Just r' -> if (r' == 0) then Nothing else Just (l' `div` r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (If0 c t e) = case (evalAEMaybe c) of
  Just c' -> if c' == 0 then (evalAEMaybe t) else (evalAEMaybe e)
  Nothing -> Nothing


-- Exercise 3
evalM :: AE -> Maybe Int
evalM (Num n) =  if n < 0 then Nothing else Just n

evalM (Plus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (l' + r')

evalM (Minus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  if l' < r' then Nothing else return (l' - r')

evalM (Mult l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (l' * r')

evalM (Div l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  if r'== 0 then Nothing else return (l' `div` r')

evalM (If0 c t e) = do
  c' <- (evalM c)
  t' <- (evalM t)
  e' <- (evalM e)
  if c' == 0 then return t' else return e'


-- Exercise 4
interpAE :: String -> Maybe Int
interpAE  p = evalM (parseAE p)


-- Test 1
evalAETest :: IO ()
evalAETest = do
  print ("evalAE Test:")
  print ("1 = " ++ show (evalAE (Num 1)))
  -- print (evalAE (Num (-4)))
  print ("1+2 = " ++ show (evalAE (Plus (Num 1) (Num 2))))
  -- print (evalAE (Plus (Num (-1)) (Num 2)))
  -- print (evalAE (Minus (Num 1) (Num 2)))
  print ("2-1 = " ++ show (evalAE (Minus (Num 2) (Num 1))))
  print ("2-2 = " ++ show (evalAE (Minus (Num 2) (Num 2))))
  -- print (evalAE (Mult (Num (-1)) (Num 2)))
  print ("3*2 = " ++ show (evalAE (Mult (Num 3) (Num 2))))
  print ("0*3 = " ++ show (evalAE (Mult (Num 0) (Num 3))))
  -- print (evalAE (Div (Num 3) (Num 0)))
  -- print (evalAE (Div (Num 3) (Num (-2))))
  print ("4/2 = " ++ show (evalAE (Div (Num 4) (Num 2))))
  print ("3 == 0 = " ++ show (evalAE (If0 (Num 3) (Num 1) (Num 0))))
  print ("0 == 0 = " ++ show (evalAE (If0 (Num 0) (Num 1) (Num 0))))
  -- print (evalAE (If0 (Num (-1)) (Num 1) (Num 0)))
  print ("-------------------------------------------------------------------------")


-- Test 2
evalAEMaybeTest :: IO ()
evalAEMaybeTest = do
  print ("evalAEMaybe Test")
  print ("1 = " ++ show (evalAEMaybe (Num 1)))
  print ("-4 = " ++ show (evalAEMaybe (Num (-4))))
  print ("1+2 = " ++ show (evalAEMaybe (Plus (Num 1) (Num 2))))
  print ("-1+2 = " ++ show (evalAEMaybe (Plus (Num (-1)) (Num 2))))
  print ("1-2 = " ++ show (evalAEMaybe (Minus (Num 1) (Num 2))))
  print ("2-1 = " ++ show (evalAEMaybe (Minus (Num 2) (Num 1))))
  print ("2-2 = " ++ show (evalAEMaybe (Minus (Num 2) (Num 2))))
  print ("-1*2 = " ++ show (evalAEMaybe (Mult (Num (-1)) (Num 2))))
  print ("3*2 = " ++ show (evalAEMaybe (Mult (Num 3) (Num 2))))
  print ("0*3 = " ++ show (evalAEMaybe (Mult (Num 0) (Num 3))))
  print ("3/0 = " ++ show (evalAEMaybe (Div (Num 3) (Num 0))))
  print ("3/-2 = " ++ show (evalAEMaybe (Div (Num 3) (Num (-2)))))
  print ("4/2 = " ++ show (evalAEMaybe (Div (Num 4) (Num 2))))
  print ("3 == 0 = " ++ show (evalAEMaybe (If0 (Num 3) (Num 1) (Num 0))))
  print ("0 == 0 = " ++ show (evalAEMaybe (If0 (Num 0) (Num 1) (Num 0))))
  print ("-1 == 0 = " ++ show (evalAEMaybe (If0 (Num (-1)) (Num 1) (Num 0))))
  print ("-------------------------------------------------------------------------")


-- Test 3
evalMTest :: IO ()
evalMTest = do
  print ("evalM Test")
  print ("1 = " ++ show (evalM (Num 1)))
  print ("-4 = " ++ show (evalM (Num (-4))))
  print ("1+2 = " ++ show (evalM (Plus (Num 1) (Num 2))))
  print ("-1+2 = " ++ show (evalM (Plus (Num (-1)) (Num 2))))
  print ("1-2 = " ++ show (evalM (Minus (Num 1) (Num 2))))
  print ("2-1 = " ++ show (evalM (Minus (Num 2) (Num 1))))
  print ("2-2 = " ++ show (evalM (Minus (Num 2) (Num 2))))
  print ("-1*2 = " ++ show (evalM (Mult (Num (-1)) (Num 2))))
  print ("3*2 = " ++ show (evalM (Mult (Num 3) (Num 2))))
  print ("0*3 = " ++ show (evalM (Mult (Num 0) (Num 3))))
  print ("3/0 = " ++ show (evalM (Div (Num 3) (Num 0))))
  print ("3/-2 = " ++ show (evalM (Div (Num 3) (Num (-2)))))
  print ("4/2 = " ++ show (evalM (Div (Num 4) (Num 2))))
  print ("3 == 0 = " ++ show (evalM (If0 (Num 3) (Num 1) (Num 0))))
  print ("0 == 0 = " ++ show (evalM (If0 (Num 0) (Num 1) (Num 0))))
  print ("-1 == 0 = " ++ show (evalM (If0 (Num (-1)) (Num 1) (Num 0))))
  print ("-------------------------------------------------------------------------")


-- Test 4
interpAETest :: IO ()
interpAETest = do
  print ("interpAE Test")
  print ("1 = " ++ show (interpAE "1"))
  print ("-4 = " ++ show (interpAE "-4"))
  print ("1+2 = " ++ show (interpAE "1+2"))
  print ("-1+2 = " ++ show (interpAE "(-1)+2"))
  print ("1-2 = " ++ show (interpAE "1-2"))
  print ("2-1 = " ++ show (interpAE "2-1"))
  print ("2-2 = " ++ show (interpAE "2-2"))
  print ("-1*2 = " ++ show (interpAE "(-1)*2"))
  print ("3*2 = " ++ show (interpAE "3*2"))
  print ("0*3 = " ++ show (interpAE "0*3"))
  print ("3/0 = " ++ show (interpAE "3/0"))
  print ("3/-2 = " ++ show (interpAE "3/(-2)"))
  print ("4/2 = " ++ show (interpAE "4/2"))
  print ("-------------------------------------------------------------------------")


-- Test all
main = do
  print ("Start the tests: ")
  evalAETest
  evalAEMaybeTest
  evalMTest
  interpAETest
  print ("Test complete!")


    