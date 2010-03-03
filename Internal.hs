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
    str <- getInput;
    (res1, _) <- return $ testParser p1 str;
    (res2, _) <- return $ testParser p2 str;
    case (ok res1, ok res2) of 
        (True, _) -> p1
        (_, True) -> p2
        _         -> orz
} where
    ok (Left (ErrParseFail _)) = False
    ok (Left (ErrEOF)) = True
    ok (Right _) = True

err :: FError -> FParser a
err e = FParser $ \state -> (Left e, state)

orz :: FParser a
orz = err $ ErrParseFail ""

testParser p [] = runParser p $ newFState ""
testParser p (c:_) = runParser p $ newFState [c]

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



