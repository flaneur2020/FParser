module Lexer where

import Data.Maybe
import Control.Monad

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

word :: String -> FParser String 
word str = do {
    --foldl (\acc x -> acc >> (char x)) (char x) xs;
    sequence $ map char $ str;
    return str;
}

parens = between (sym "(") (sym ")")

sym = word

