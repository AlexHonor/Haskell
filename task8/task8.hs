module Main where

import Text.Read ( readMaybe )
import System.Environment
import System.IO

addStuff :: [String] -> Float -> String -> IO()
addStuff [] accum text = do print text 
                            print (show accum)
                            outh <- openFile "output.txt" WriteMode
                            hPrint outh text
                            hPrint outh (show accum)
                            hClose outh
addStuff (line:lines) accum text = case readMaybe line of 
                                        Just n ->  addStuff lines (accum + n) text
                                        Nothing -> addStuff lines accum (text ++ line)

main :: IO ()
main = do xs <- getArgs
          addStuff xs 0 ""