module Main where

main :: IO ()
main = do xs <- getLine
          print (filterBrackets xs)
          if depth (filterBrackets xs) [] 0 < 0
              then putStrLn "-1"
              else putStrLn (show (depth (filterBrackets xs) [] 0))

isOpeningBracket :: Char -> Bool
isOpeningBracket c = c == '{' || c == '[' || c == '<' || c == '('

isClosingBracket :: Char -> Bool
isClosingBracket c = c == '}'   || c == ']' || c == '>' || c == ')'

isBracket :: Char -> Bool
isBracket c = isOpeningBracket c || isClosingBracket c

isMatching :: Char -> Char -> Bool
isMatching a b = (a == '{') && (b == '}') || (a == '[') && (b == ']') || (a == '<') && (b == '>') || (a == '(') && (b == ')')

filterBrackets :: [Char] -> [Char]
filterBrackets [] = []
filterBrackets (x: xs) | isBracket x = x : filterBrackets xs
                       | otherwise   = filterBrackets xs

depth :: [Char] -> [Char] -> Int -> Int
depth [] [] n = n
depth [] _ _ = -1
depth (x: xs) ss n | isOpeningBracket x = depth xs (x:ss) (max n (1 + length ss))
                   | isClosingBracket x && isMatching (head ss) x  = depth xs (tail ss) n
                   | otherwise = -1
