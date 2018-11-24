
import Data.Char
import Control.Monad

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing


main :: IO ()
main = do
    line <- getLine
    if (length (line :: String) == 0) then
        Prelude.return ()
    else do
        print (filterM (\_ -> [True,False]) [1,2,3] :: [[Integer]])
        main
