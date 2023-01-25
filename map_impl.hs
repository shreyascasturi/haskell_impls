-- impl of map operator
map_new :: (a -> b) -> [a] -> [b]
map_new f [] = []
map_new f (x:xs) = [f x] ++ (map_new f xs)

-- if int is even, then True, else False
int_to_even_bool :: Int -> Bool
int_to_even_bool x = if (x `mod` 2) == 0 then True else False

-- see which ints in list are even or not
test_list = [20, 30, 40, 50]
main = do
putStrLn "hello world"
print (map_new int_to_even_bool test_list)
