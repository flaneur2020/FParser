{-# OPTIONS_GHC  #-}

module FParser where

import Control.Monad
import Data.Maybe

newtype FState = FState {
    input :: String 
} deriving (Show)

newtype FParser a = FParser { 
    runParser :: (FState -> (Maybe a, FState)) 
}
        
parse p str = runParser p $ newFState str

--rawGetInput :: FParser a -> String 
--rawGetInput p = 

newFState str = FState $ str

getState :: FParser FState
getState = FParser $ \state -> (Just state, state)

getInput :: FParser String
getInput = FParser $ \state -> (Just $ input state, state)

setInput :: String -> FParser String
setInput str = FParser $ \_ -> (Just str, FState str) 


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

nop :: FParser ()
nop = return ()

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


oneOf :: String -> FParser Char 
oneOf [] = orz
oneOf (x:xs) = do {
    foldl (\acc x -> acc <|> (char x)) (char x) $ xs;
}

digit = oneOf "0123456789"

number :: FParser Integer
number = do {
    str <- many1 digit;
    return $ read $ str;
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
    xs <- many1 $ do {
        f <- op; 
        val <- p;
        return (`f` val);
    };
    return $ foldl (\acc f -> f acc) x xs;
} <|> (do {
    p;
})

between :: FParser a -> FParser b -> FParser c -> FParser c
between open close p = do {
    open;
    v <- p;
    close;
    return v;
}

parens = between (sym "(") (sym ")")

space = char ' '
tab   = char '\t'
ignore = many $ space <|> tab 

word :: String -> FParser String 
word str = do {
    --foldl (\acc x -> acc >> (char x)) (char x) xs;
    sequence $ map char $ str;
    return str;
}

sym = word

test_word= parse (do {
    (word "test") <|> (word "orz");
}) 

test_many str = runParser (do {
    many $ char 'a';
}) $ newFState str

test_oneOf = parse (do {
    many1 $ oneOf "abc"
})  

test_num = parse (do {
    n<-number;    
    return $ n;
})

test_blah = parse (do {
    number;    
    sym "+";
    number;
})

test_parens = parse (do {
    parens $ number;  
})

expr = term `chain` addop
term = factor `chain` mulop
factor = (parens expr) <|> number
mulop =  do { sym "*"; return (*); }
     <|> do { sym "/"; return (div);}
addop =  do { sym "+"; return (+); }
     <|> do { sym "-"; return (-);}
test_arith = parse expr
