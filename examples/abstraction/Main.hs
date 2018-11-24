
import Prelude hiding (Maybe, Nothing, Just, pure, (<*>), Monad)

inc' :: Num a => [a] -> [a]
inc' [] = []
inc' (n:ns) = n + 1 : inc' ns

sqr' :: Num a => [a] -> [a]
sqr' [] = []
sqr' (n:ns) = n * n : sqr' ns

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x:xs)   = f x : map' f xs

inc'' :: Num a => [a] -> [a]
inc'' = map' (+1)

sqr'' :: Num a => [a] -> [a]
sqr'' = map' (\x -> x * x)

data Maybe a = Nothing | Just a
    deriving (Show, Eq)

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b

instance MyFunctor [] where
    fmap' = map

instance MyFunctor Maybe where
    fmap' _ Nothing = Nothing
    fmap' g (Just x) = Just (g x)

class MyFunctor f => MyApplicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance MyApplicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just g) <*> mx = fmap' g mx

instance MyApplicative [] where
    pure x = [x]
    gs <*> xs = [g x | g <- gs, x <- xs]

class MyApplicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

    return = pure

products :: Num a => [a] -> [a] -> [a]
products xs ys = [x * y | x <- xs, y <- ys]

products' :: Num a => [a] -> [a] -> [a]
products' xs ys = pure (*) <*> xs <*> ys

type State = Int

newtype StateTransformer a = S (State -> (a, State))

app :: StateTransformer a -> State -> (a, State)
app (S st) x = st x

instance MyFunctor StateTransformer where
    fmap' g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance MyApplicative StateTransformer where
    pure x = S (\s -> (x, s))

    stf <*> stx = S (\s ->
        let (f, s')  = app stf s
            (x, s'') = app stx s' in (f x, s''))

instance Monad StateTransformer where
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

main :: IO ()
main = do
    print (fmap' (+1) Nothing  :: Maybe Integer)
    print (fmap' (*2) (Just 3) :: Maybe Integer)

    print (pure (+1) <*> [1..15] :: [Integer])
    print (pure (+) <*> [1] <*> [2] :: [Integer])
    print (pure (*) <*> [1,2] <*> [3,4] :: [Integer])
    print (pure (*) <*> [2,3,4] <*> [3,4,5] :: [Integer])

    print (products  [1..15] [1..15] :: [Integer])
    print (products' [1..15] [1..15] :: [Integer])
