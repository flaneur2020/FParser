module Type where

data FError = ErrParseFail String
            | ErrEOF
            | NoError
            deriving (Show)

data FState = FState {
    pos :: (Int, Int),
    input :: String
} deriving (Show)

data FParser a = FParser { 
    runParser :: (FState -> (Either FError a, FState))
}

instance Monad FParser where 
    return v = 
        FParser {
            runParser = \state -> (Right v, state)
        }

    -- (>>=) :: FParser a -> (a -> FParser b) -> FParser b  
    (>>=) p f = 
        FParser $ \state -> 
            let 
                (m, new_state) = runParser p state 
            in case m of 
                Right v  -> runParser (f v) new_state 
                Left err -> (Left err, state)

newFState str = FState { 
    input = str,
    pos = (0, 0)
}

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

setPos :: (Int, Int) -> FParser (Int, Int)
setPos p = do {
    modifyState $ \s -> s { pos = p };
    return p;
}

getPos :: FParser (Int, Int)
getPos = do {
    state <- getState;
    return $ pos $ state;
}
