module Combinator where

import Data.Maybe
import Control.Monad

import Type
import Internal

nop :: FParser ()
nop = return ()

char :: Char -> FParser Char
char c = satisfy (==c)

oneOf :: String -> FParser Char 
oneOf str = satisfy (`elem` str) 

many :: FParser a -> FParser [a]
many p = many' [] p
many' r p = do {
    str <- getInput;
    case str of 
        [] -> return r
        _  -> 
            case (testParser p str) of 
                (Left (ErrParseFail _), _) -> return r
                _ -> (do {
                    val <- p;
                    many' (r++[val]) p;
                })
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





