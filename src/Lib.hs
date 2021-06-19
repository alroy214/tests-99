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

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = myLength xs + 1


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = compressHelper xs x []

compressHelper :: (Eq a) => [a] -> a -> [a] -> [a]
compressHelper [] _ acc = acc
compressHelper (x:xs) prev acc
  | x == prev = compressHelper xs prev acc
  | otherwise = compressHelper xs x (acc ++ [x])

pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = packHelper xs x [x] []

packHelper :: (Eq a) => [a] -> a -> [a] -> [[a]] -> [[a]]
packHelper [] _ [] acc = acc
packHelper [] _ curr acc = acc ++ [curr]
packHelper (x:xs) prev curr acc
  | x == prev = packHelper xs x (x : curr) acc
  | otherwise = packHelper xs x [x] (acc ++ [curr])

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode ls = map magic (pack ls)

magic :: [a] -> (Int, a)
magic (x:xs) = (length (x:xs), x)

data ListItem a = Multiple Int a | Single a deriving Show

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified list = map encodeModifiedHelper(encode list)

encodeModifiedHelper :: (Int, a) -> ListItem a
encodeModifiedHelper (1, x) = Single x
encodeModifiedHelper (n, x) = Multiple n x


decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified list = decodeHelper2 (map decodeHelper list) []

decodeHelper2 :: [[a]] -> [a] -> [a]
decodeHelper2 [] acc = acc
decodeHelper2 (x:xs) acc = decodeHelper2 xs (acc ++ x)

decodeHelper :: ListItem a -> [a]
decodeHelper (Single x) = [x]
decodeHelper (Multiple n x) = replicate n x

someFunc :: IO ()
someFunc =  print (decodeModified 
                          [Multiple 4 'a',Single 'b',Multiple 2 'c',
                           Multiple 2 'a',Single 'd',Multiple 4 'e'])
