module Parsers (eval, clear, negative) where

import Control.Applicative
import Data.Char
import Text.Read
import Numeric

newtype Parser a = P (String -> [(a,String)])  

parse :: Parser a ->String -> [(a,String)]
parse (P p) content = p content  

instance Functor Parser where
    fmap f p =P (\content -> case parse p content of
                            [] -> []
                            [(m,n)] -> [(f m,n)])

instance Applicative Parser where
    pure m = P(\content -> [(m,content)])
    px <*> py =P(\content -> case parse px content of
                            [] ->[]
                            [(x,n)] -> parse (fmap x py) n ) 

instance Monad Parser where
    p >>= f = P(\content ->case parse p content of
                             [] ->[]
                             [(m,n)]->parse (f m) n)

instance Alternative Parser where
    empty = P(\content -> [])
    p <|> q = P(\content -> case parse p content of
                            [] -> parse q content
                            [(m,n)] -> [(m,n)])
                            
item :: Parser Char
item = P (\content -> case content of
                      [] -> []
                      (x:xs) -> [(x,xs)])

sel :: (Char->Bool) -> Parser Char
sel p = do x <-item
           if p x then return x else empty 

digit :: Parser Char
digit = sel isDigit

char :: Char ->Parser Char
char x = sel (==x)

decimal :: Parser Float
decimal = do xs <- some digit
             char '.'
             ys <- some digit
             return (read $ xs++"."++ys ::Float)
            <|> do zs <- some digit  
                   return (read zs ::Float)

decimalx ::Parser Float
decimalx = do char '-'
              x <- decimal
              return (-x)  
              <|> decimal   


expr :: Parser Float
expr = do t<-term
          do 
            do char '+'
               e <- expr
               return (t+e)
               <|> do char '-'
                      e<-expr
                      return (t-e)
            <|> return t  

term :: Parser Float
term = do f<-factor
          do
           do char '*'
              t <- term
              return (f*t)
              <|> do char '/'
                     t <- term
                     return (f/t)
            <|> return f 

factor :: Parser Float
factor = do char '('
            do char '-'
               e <-expr
               char ')'
               return (-e)
               <|>do e <-expr
                     char ')' 
                     return e
            <|> decimalx  
   
        
eval :: String ->String
eval xs = case (parse expr xs) of
                        [(n,[])] -> showFFloat Nothing n ""
                        [(_,out)] ->error("incorrect contentut:" ++out)

clear :: String -> String
clear _ = []

negative :: String -> String
negative xs = "(" ++show ( -read xs ::Double) ++ ")" 
