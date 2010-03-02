module Combinator where

import Data.Maybe
import Control.Monad

import Internal

nop :: FParser ()
nop = return ()

oneOf :: String -> FParser Char 
oneOf [] = orz "oneOf"
oneOf (x:xs) = do {
    foldl (\acc x -> acc <|> (char x)) (char x) $ xs;
}

many :: FParser a -> FParser [a]
many p = many' [] p
many' r p = do {
    ok <- test p;
    if ok then (do {
        val <- p;
        many' (r++[val]) p;
    })
    else 
        return r;
}

many1 :: FParser a -> FParser [a]
many1 p = do {
    first <- p;
    rest <- many p;
    return $ first:rest;
}

chain :: FParser a -> FParser (a -> a -> a) -> FParser a
chain p op = do {
    x <- p;
    xs <- many $ do {
        f <- op; 
        val <- p;
        return (`f` val);
    };
    return $ foldl (\acc f -> f acc) x xs;
} 

between :: FParser a -> FParser b -> FParser c -> FParser c
between open close p = do {
    open;
    v <- p;
    close;
    return v;
}





