module Test where

import Control.Monad
import Data.Maybe

import Internal
import Combinator
import Lexer

test_word= parse (do {
    (word "test") <|> (word "orz");
}) 

test_many str = runParser (do {
    many $ char 'a';
}) $ newFState str

test_oneOf = parse (do {
    many1 $ oneOf "abc"
})  

test_num = parse (do {
    n<-number;    
    return $ n;
})

test_blah = parse (do {
    number;    
    sym "+";
    number;
})

test_parens = parse (do {
    parens $ number;  
})

expr = term `chain` addop
term = factor `chain` mulop
factor = (parens expr) <|> number
mulop =  do { sym "*"; return (*); }
     <|> do { sym "/"; return (div);}
addop =  do { sym "+"; return (+); }
     <|> do { sym "-"; return (-);}
test_arith = parse expr

main = do {
    print $ test_arith "1+1"
}
