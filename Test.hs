module Test where

import Control.Monad
import Data.Maybe

import Internal
import Combinator
import Lexer



test_oneOf = parse (do {
    many1 $ oneOf "abcd"
}) $ "abbbaafdfdfdf"

test_blah = parse (do {
    number;    
    addop;
    number;
}) $ "1+2"

test_blah2 = parse (do {
    number;
    many $ do {
        addop;
        number;
    }
}) $ "1+2+2+2"

test_parens = parse (do {
    parens $ parens $ number;  
}) $ "((12))"

test_chain = parse (do {
    exp_;
}) $ "1+1"
exp_ = number `chain` addop;

test_many = parse (do {
    --many $ sym "test";    
    many $ sym "test ";    
}) $ "test test test  "

expr = term `chain` addop
term = factor `chain` mulop
factor = (parens expr) <|> number
mulop =  do { sym "*"; return (*); }
     <|> do { sym "/"; return (div);}
addop =  do { sym "+"; return (+); }
     <|> do { sym "-"; return (-);}
test_arith = parse expr

main = do {
    print $ test_arith "1+ ";
    print $ test_arith "((2*12+1*2)*2)*3 ";
    print $ test_many;
}

