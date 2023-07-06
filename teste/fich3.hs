--- 3.1)

funao :: (a->b) -> (a->Bool) -> [a] -> [b]
funao f g l = map f (filter g l)

--- 3.2)

dec2int :: [Int] -> Int
dec2int l = foldl (\acc x -> acc*10 + x) 0 l


--- 3.3)
myzipWith :: (a → b → c) -> [a] -> [b] -> [c]
myzipWith f [] [] = f [] [] 
myzipWith f (x:xs) (y:ys)= (f x y):(myzipWith f xs ys)