module Main where

import System.Environment
import System.IO

call :: String -> Float -> Float
call "sin" = sin
call "cos" = cos
call "tan" = tan
call "log" = log
call "sqrt" = sqrt
call "tanh" = tanh
call "sinh" = sinh

funPrint :: String -> Float -> Float -> Float -> IO ()
funPrint func a b h | a <= b = do print (show (call func a))
                                  funPrint func (a + h) b h
                    | otherwise = return ()

main :: IO ()
main = do xs <- getArgs
          funPrint (head xs) (read (xs !! 1) :: Float) (read (xs !! 2) :: Float) (read (xs !! 3) :: Float)
