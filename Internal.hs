module Internal where

import Control.Monad
import System.IO.Unsafe
import System.IO

import Type

parse p str = 
    let (r, s) = runParser p $ newFState str 
    in (r, s)

(<|>) :: FParser a -> FParser a -> FParser a
(<|>) p1 p2 = do {
    ok1 <- test p1;
    ok2 <- test p2;
    str <- getInput;
    case (ok1, ok2) of 
        (True, _) -> p1
        (_, True) -> p2
        (_, _) -> orz 
}

err :: FError -> FParser a
err e = FParser $ \state -> (Left e, state)

orz :: FParser a
orz = err $ ErrParseFail ""


test :: FParser a -> FParser Bool
test p = do {
    tmp_state <- getState;
    case (input tmp_state) of
        [] -> return True
        _input -> test_char _input p 
}

testParser p [] = runParser p $ newFState ""
testParser p (c:_) = runParser p $ newFState [c]

test_char :: String -> FParser a -> FParser Bool
test_char (c:_) p = test_str [c] p 

test_str :: String -> FParser a -> FParser Bool
test_str [] p  = return True
test_str str p = do {
    (m, new_state) <- return $ runParser p $ newFState str;
    return $ case m of 
        Right _     -> True
        Left ErrEOF -> True
        Left _      -> False
}

-- root of atom!
satisfy :: (Char -> Bool) -> FParser Char
satisfy f = do {
    state   <- getState;
    _input  <- return $ input $ state;
    (x, y)  <- return $ pos $ state;
    case _input of 
        (v:rest) -> 
            if (f v) then (do {
                -- inc the line number
                if (v=='\n') then setPos $ (0, y+1)
                             else setPos $ (x+1, y);
                -- go on~
                setInput rest;
                return v;
            })
            else orz; 
        [] -> err $ ErrEOF;
}

char :: Char -> FParser Char
char c = satisfy (==c)



