
import Data.Char

length' :: Integral b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

lsum :: Num a => [a] -> a
lsum [] = 0
lsum (x:xs) = x + lsum xs

popcar :: [a] -> [a]
popcar [] = []
popcar (_:xs) = xs

pushcar :: a -> [a] -> [a]
pushcar i [] = [i]
pushcar i l = [i] ++ l 

showAll :: Show a => [a] -> [String]
showAll [] = []
showAll (x:xs) = show x : showAll xs

square :: Num a => a -> a
square n = n * n

map' :: Num a => (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

l2 :: [Integer]
l2 = [1..15]

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' xss = [x | xs <- xss, x <- xs]

l3 :: [[Integer]]
l3 = [[1,2,3],[4,5,6],[7,8,9]]

factors' :: Integral a => a -> [a]
factors' n = [x | x <- [1..n], n `mod` x == 0]

prime :: Integral a => a -> Bool
prime n = factors' n == [1,n]

primesUpTo :: Integral a => a -> [a]
primesUpTo n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

getFirst :: [a] -> a
getFirst [] = error "<List is empty>"
getFirst (x:_) = x

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- Source: Oxford Math Center
-- (http://www.oxfordmathcenter.com/drupal7/node/353)
table :: [Double]
table = [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,6.327,9.056,2.758,0.978,2.360,0.0150,1.974,0.074]

percent :: Int -> Int -> Double
percent n d = (fromIntegral n) / (fromIntegral d) * 100

frequencies :: String -> [Double]
frequencies xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chiSq :: [Double] -> [Double] -> Double
chiSq os es = sum [((o - e)^(2 :: Integer))/e | (o,e) <- zip os es]

fac :: Integral a => a -> a
fac n = product [1..n]

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

listA :: [Integer]
listA = [1..26]

listB :: [Char]
listB = ['a'..'z']

even' :: Integral a => a -> Bool
even' 0 = True
even' n = odd (n - 1)

odd' :: Integral a => a -> Bool
odd' 0 = False
odd' n = even (n - 1)

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

productLong :: Num a => [a] -> a
productLong l = foldr (*) 1 l

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs

dropFirstN :: Integral a => a -> [b] -> [b]
dropFirstN _ [] = []
dropFirstN 0 l = l
dropFirstN n (_:xs) = dropFirstN (n - 1) xs

listOfThings :: [String]
listOfThings = ["A","B","C","D","E","F"]

doTwice :: (a -> a) -> a -> a
doTwice f x = (f . f) x

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

sum' :: Num a => [a] -> a
sum' = sum'' 0
    where
        sum'' v [] = v
        sum'' v (x:xs) = sum'' (v + x) xs

length'' :: Integral a => [b] -> a
length'' = foldl (\n _ -> n + 1) 0

data Perhaps a = NaryAThing | ASingular a
    deriving (Show, Eq)

divideSafely :: Integral a => a -> a -> Perhaps a
divideSafely _ 0 = NaryAThing
divideSafely n d = ASingular (div n d)

data Nat = Zero | Succ Nat
    deriving (Show, Eq)

nat2int :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

data List a = Nil | Cons a (List a)

len :: Integral a => List b -> a
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r



main :: IO ()
main = do
    -- print $ (length' l2 :: Integer)
    -- print $ (lsum l2)
    -- print $ (popcar l2)
    -- print $ (pushcar 420 l2)
    -- print $ (showAll l2)
    -- print $ (map' square l2)
    -- print $ (map' (\n -> n * n * n) l2)
    -- print $ ([x^4| x <- [1..15]] :: [Integer])
    -- print $ concat' l3
    -- print $ (factors' 17 :: [Integer])
    -- print $ l2
    -- print $ map' prime l2
    -- print $ (primesUpTo 700 :: [Integer])
    -- print $ frequencies "abbcccddddeeeee"
    -- print $ (sum [x*x | x <- [1..100]] :: Integer)
    -- print (map' fac [1..10] :: [Integer])
    -- print (product' [1..17] :: Integer)
    -- print (zip' listA listB :: [(Integer,Char)])
    -- print (map' even' [1..10] :: [Bool])
    -- print (product'' [1..15] :: Integer)
    -- print (productLong [1..15] :: Integer)
    -- print (listOfThings :: [String])
    -- print (dropFirstN (4 :: Integer) (listOfThings))
    -- print (doTwice (\x -> x + 1) 0 :: Integer)
    -- print (mapl (\x -> x^(2 :: Integer)) [1..25] :: [Integer])
    -- print (filter (\x -> x `mod` 2 == 0) [1..25] :: [Integer])
    -- print (sum' [1..57] :: Integer)
    -- print (length'' ['a'..'z'] :: Integer)

    -- print $ "N = " ++ (show (15 :: Integer))
    -- print $ "D = " ++ (show  (5 :: Integer))
    -- print (divideSafely (15 :: Integer) (5 :: Integer) :: Perhaps Integer)
    
    -- print (mapl (\n -> nat2int n) [Zero, Succ Zero, Succ (Succ Zero)])

    -- print (occurs (7 :: Int) t :: Bool)
    -- print (occurs (5 :: Int) t :: Bool)
    -- print (occurs (6 :: Int) t :: Bool)
    -- print (occurs (0 :: Int) t :: Bool)

    print (flatten t :: [Int])
