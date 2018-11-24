
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    []      -> []
                    (x:xs)  -> [(x,xs)])

instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of
                            []          -> []
                            [(v, out)]  -> [(g v, out)]
                            ((_,_):_:_) -> error "1.")

instance Applicative Parser where
    pure v = P (\inp -> [(v, inp)])

    pg <*> px = P (\inp -> case parse pg inp of
                    []          -> []
                    [(g, out)]  -> parse (fmap g px) out
                    ((_,_):_:_) -> error "2.")

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
    where
        g x _ z = (x,z)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> parse (f v) out
                            ((_,_):_:_) -> error "3.")

three' :: Parser (Char, Char)
three' = do
    x <- item
    _ <- item
    z <- item
    return (x,z)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v,out)] -> [(v,out)]
                            ((_,_):_:_) -> error "4.")

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then
        return x
    else
        empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []       = return []
string (x:xs)   = do
    _ <- char x
    _ <- string xs
    return (x:xs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    _ <- many (sat isSpace)
    return ()

int :: Parser Int
int = do
    _ <- char '-'
    n <- nat
    return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
    _ <- space
    v <- p
    _ <- space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Int
expr =  do t <- term
           do _ <- symbol "+"
              e <- expr
              return (t + e)
            <|> return t

term :: Parser Int
term = do f <- factor
          do _ <- symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do _ <- symbol "("
            e <- expr
            _ <- symbol ")"
            return e
        <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_,out)] -> error ("Unused input: " ++ out)
            [] -> error "Invalid input"
            _ -> error "Invalid input"

peval :: String -> IO ()
peval str = print (eval str)

main :: IO ()
main = do
    peval "2*3+4"
    peval "2*(3+4)"
    --peval "2*3^4" -- Error: Unused input ^4
    --peval "one plus two" -- Error: Invalid input
