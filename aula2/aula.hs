(@@) :: (Integral b) => [a] -> b -> a
[] @@ _ = error "empty list!"
(x:_) @@ 0 = x
(x:xs) @@ n
    | n > 0 = xs @@ (n-1)
    | n < 0 = error "negative integer argument"

(@@@) :: (Integral b) => [a] -> b -> a
l @@@ n = head [x | (x,i) <- zip l [0 ..], i == n]


concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ (concat' t)

concat'' :: [[a]] -> [a]
concat'' l = [x | xs <- l, x <- xs]



quads :: (Integral a) => [a] -> a
quads [] = 0
quads (x:xs) = x^2 + quads xs

replicate' :: (Integral a) => a -> b -> [b]
replicate' 0 _ = []
replicate' n x
    | n > 0 = x:(replicate' (n-1) x)
    | otherwise = error "negative integer argument"

replicate'' :: (Integral a) => a -> b -> [b]
replicate'' n x = [x | _ <- [1 .. n]]



reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x] 

reverse'' :: [a] -> [a]
reverse'' l = reverse''Aux l []

reverse''Aux :: [a] -> [a] -> [a]
reverse''Aux [] acc = acc
reverse''Aux (x:xs) acc = reverse''Aux xs (x:acc)
