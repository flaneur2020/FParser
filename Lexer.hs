module Lexer where

import Data.Maybe
import Control.Monad

import Type
import Internal
import Combinator

number :: FParser Integer
number = do {
    str <- many1 digit;
    return $ read $ str;
}

digit = oneOf "0123456789"
space = char ' '
tab   = char '\t'
ignore = many $ space <|> tab 

sym :: String -> FParser String 
sym str = do {
    --foldl (\acc x -> acc >> (char x)) (char x) xs;
    res <- sequence $ map char $ str;
    return res;
}

parens = between (sym "(") (sym ")")


