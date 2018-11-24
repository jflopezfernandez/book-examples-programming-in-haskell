
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

main :: IO ()
main = do
    print (parse item "")
    print (parse item "abc")
    print (parse (fmap toUpper item) "abc")
    print (parse (fmap toUpper item) "")
    print (parse (pure (1 :: Integer)) "abc")
    print (parse three "abcdef")
    print (parse three "ab")
