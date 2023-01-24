head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

tail' :: [a] -> Maybe a
tail' [] = Nothing
tail' [y] = Just y
tail' (x:xs) = tail' xs

get' :: [a] -> Int -> Maybe a
get' [] _ = Nothing
get' (x:xs) 0 = Just x
get' (x:xs) b = get' xs (b - 1)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [y] = [y]
reverseList (x:xs) = 

main = do
  print (head' [3])
  print (tail' [2])
  print (get' [1,2,3] 2)
  print (length' [1,2,3,4,6,5])
