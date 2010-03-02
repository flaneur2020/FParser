module Internal where

import Control.Monad
import System.IO.Unsafe
import System.IO

-- TODO : refactor (a, FSTate)
-- FSTate { input , err }

data FError = ErrParseFail String
            | ErrEOF
            | NoError
            deriving (Show)

newtype FState = FState {
    input :: String 
} deriving (Show)

newtype FParser a = FParser { 
    runParser :: (FState -> (Either FError a, FState)) 
}
        
parse p str = 
    let (r, s) = runParser p $ newFState str 
    in (r, s)

newFState str = FState $ str

getState :: FParser FState
getState = FParser $ \state -> (Right state, state)

setState :: FState -> FParser FState
setState state = FParser $ \_ -> (Right state, state) 

modifyState :: (FState -> FState) -> FParser FState
modifyState f = do {
    state <- getState;
    setState $ f state;
}

getInput :: FParser String
getInput = do {
    state <- getState;
    return $ input state;
} 

setInput :: String -> FParser String
setInput str = do {
    modifyState $ \s -> s { input = str };
    return str;
}


instance Monad FParser where 

    return v = 
        FParser $ \state -> (Right v, state)

    -- (>>=) :: FParser a -> (a -> FParser b) -> FParser b  
    (>>=) p f = 
        FParser $ \state -> 
            let 
                (m, new_state) = runParser p state 
            in case m of 
                Right v  -> runParser (f v) new_state 
                Left err      -> (Left err, state)


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
        _input@(c:_) -> (do {
            (m, new_state) <- return $ runParser p $ newFState [c];
            setState $ tmp_state;
            return $ case m of 
                Right _     -> True
                Left ErrEOF -> True
                Left _      -> False
        })
}

-- root of atom!
char :: Char -> FParser Char
char c = do {
    str <- getInput;
    case str of 
        (v:rest) -> 
            if (c==v) then (do {
                -- TODO : count the line number
                setInput rest;
                return c;
            })
            else orz $ "orz1"++(show (c, v));
        [] -> err $ ErrEOF;
} 



