factorial n =  product [1..n]

average ns = sum ns `div` length ns

customLast xs = head (reverse xs)
customLast2 :: [a] -> a
customLast2 xs = xs !! (length xs - 1)

customInit xs = reverse (drop 1 (reverse xs))
customInit2 xs = reverse  (tail (reverse xs))

second xs = head (tail xs)

swap (x,y) = (y, x)

pair x y = (x,y)

double x = x*2

quadruple x = double (double x)

palindrome xs = reverse xs == xs

twice f x = f (f x)

odds n = map (\x -> x*2 + 1) [0..n-1]


-- Pattern matching variant

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- Conditional expressions

safetail2 :: [a] -> [a]
safetail2 xs = if null xs then [] else tail xs

-- Guarded equations

safetail3 :: [a] -> [a]
safetail3 xs | null xs = []
             | otherwise = tail xs

isPyth :: Int -> Int -> Int -> Bool
isPyth x y z | x^2 + y^2 == z^2 = True 
             | otherwise = False

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], isPyth x y z]

isDivisor :: Int -> Int -> Bool
isDivisor a b = a `mod` b == 0

factors :: Int -> [Int]
factors n = [x | x <- [1..n], isDivisor n x]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

dotProduct :: [Int] -> [Int] -> Int 
dotProduct xs ys = sum [uncurry (*) p | p <- zip xs ys]

customAnd :: [Bool] -> Bool 
customAnd [] = True
customAnd [x] = x
customAnd (x: xs) = x && customAnd xs

customConcat :: [[a]] -> [a] 
customConcat [] = []
customConcat (xs: xss) = xs ++ concat xss

customReplicate :: Int -> a -> [a]
customReplicate 0 _ = []
customReplicate n x = x: customReplicate (n-1) x


customIndex :: [a] -> Int -> a
customIndex (x: xs) 0 = x
customIndex (x: xs) n = customIndex xs (n - 1)

customElem :: Eq a => a -> [a] -> Bool
customElem elem [] = False
customElem elem (x:xs) = x == elem || customElem elem xs

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y: ys) | x < y     = x : merge xs (y:ys)
                     | otherwise = y : merge (x:xs) ys