module Lib
    ( someFunc
    ) where

myLast :: [a] -> a
myLast [] = error "boop"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "boop"
myButLast [x] = error "still boop"
myButLast (x:xs) = if length xs == 1 then x
                   else myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "boop"
elementAt (x:_) 1 = x
elementAt (x:xs) b 
    | b < 1 = error "very boop"
    | otherwise = elementAt xs (b-1)
    
myLength :: [a] -> 


someFunc :: IO ()
someFunc =  print (elementAt "haskell" 5)
