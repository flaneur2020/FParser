module Internal where

import Control.Monad
import Data.Maybe

newtype FState = FState {
    input :: String 
} deriving (Show)

newtype FParser a = FParser { 
    runParser :: (FState -> (Maybe a, FState)) 
}
        
parse p str = runParser p $ newFState str

newFState str = FState $ str

getState :: FParser FState
getState = FParser $ \state -> (Just state, state)

setState :: FState -> FParser FState
setState state = FParser $ \_ -> (Just state, state) 

getInput :: FParser String
getInput = do {
    state <- getState;
    return $ input state;
} 

setInput :: String -> FParser String
setInput str = do {
    state <- getState;
    setState $ newFState str;
    return str;
}


instance Monad FParser where 

    return v = 
        FParser $ \state -> (Just v, state)

    -- (>>=) :: FParser a -> (a -> FParser b) -> FParser b  
    (>>=) p f = 
        FParser $ \state -> 
            let 
                (m, new_state) = runParser p state 
            in case m of 
                Just v  -> runParser (f v) new_state 
                Nothing -> (Nothing, state) 


(<|>) :: FParser a -> FParser a -> FParser a
(<|>) p1 p2 = do {
    m1 <- test p1;
    m2 <- test p2;
    case (m1, m2) of 
        (True, _) -> p1
        (_, True) -> p2
        (_, _) -> orz
}

orz :: FParser a
orz = FParser $ \state -> (Nothing, state)

test :: FParser a -> FParser Bool
test p = do {
    state <- getState;
    (m, _) <- return $ runParser p state;
    case m of 
        Just _  -> return True
        Nothing -> return False
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
            else orz;
        [] -> orz;
} 



