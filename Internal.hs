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
        (_, _) -> orz $ "orz (" 
}

err :: FError -> FParser a
err e = FParser $ \state -> (Left e, state)

orz :: String -> FParser a
orz str = err $ ErrParseFail str

test :: FParser a -> FParser Bool
test p = do {
    tmp_state <- getState;
    case (input tmp_state) of
        [] -> return True
        _input -> test_char _input p 
}

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
char :: Char -> FParser Char
char c = do {
    state   <- getState;
    str     <- return $ input $ state;
    (x, y)  <- return $ pos $ state;
    case str of 
        (v:rest) -> 
            if (c==v) then (do {
                -- inc the line number
                if (c=='\n') then setPos $ (0, y+1)
                             else setPos $ (x+1, y);
                setInput rest;
                return c;
            })
            else orz $ "orz1"++(show (c, v));
        [] -> err $ ErrEOF;
} 



