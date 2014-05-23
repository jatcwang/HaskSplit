module HaskSplit.Util where

import System.Random

uniqueRandom :: StdGen -> Int -> [a] -> [a]
uniqueRandom g count pool
    | count <= 0           = []
    | length pool < count  = error "pool of possible values too small"
    | otherwise            = 
        let (i , g') = randomR (0, length pool - 1) g 
            x = pool !! i
            pool' = pool `removeAt` i
        in x:uniqueRandom g' (count - 1) pool'

removeAt :: [a] -> Int -> [a]
removeAt xs i =
    let (ys,zs) = splitAt i xs   
    in   ys ++ (tail zs)

-- a map function that ignores the ith element
nmap :: (a -> b) -> [a] -> Int -> [b]
nmap _ []     _ = []
nmap f (_:xs) 0 = nmap f xs (-1)
nmap f (x:xs) i = (f x):nmap f xs (i-1)

-- a foldr1 that ignores the ith element
nfoldr1 :: Show a => (a -> a -> a) -> [a] -> Int -> a --remove Show a
nfoldr1 _ []      _ = error "empty list"
nfoldr1 _ (x:[_]) 1 = x
nfoldr1 f (x:xs)  i 
    | null xs   = x
    | i == 0    = nfoldr1 f xs (i-1)
    | otherwise = f x (nfoldr1 f xs (i-1))


