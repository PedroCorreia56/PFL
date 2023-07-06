mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p l = map f (filter p l)

dec2int :: Integral a => [a] -> a
dec2int l = foldl (\acc x -> acc*10 + x ) 0 l

func'1 a b = a * b 
func'2 a b = (*) a b 
--eta reduction
func'3 a = (*) a 
--eta reduction
func'4 = (*) 


dec2int'1 :: Integral a => [a] -> a
dec2int'1 = foldl (\acc x -> acc*10 + x ) 0

dec2int'2 :: Integral a => [a] -> a
dec2int'2 = foldl (\acc x -> (+) (acc*10) x) 0

dec2int'3 :: Integral a => [a] -> a
dec2int'3 = foldl (\acc -> (+) (acc*10)) 0

dec2int'4 :: Integral a => [a] -> a
dec2int'4 = foldl (\acc -> (+) ((*10) acc)) 0

dec2int'5 :: Integral a => [a] -> a
dec2int'5 = foldl (\acc -> ((+) . (*10)) acc) 0

dec2int'6 :: Integral a => [a] -> a
dec2int'6 = foldl ((+) . (*10)) 0

-- 3.7c --
reverseRight :: [a] -> [a]
reverseRight l = foldr (\x acc -> acc ++ [x]) [] l

-- 3.7d --
reverseLeft :: [a] -> [a]
reverseLeft l = foldl (\acc x -> x:acc) [] l

reverseLeft'1 :: [a] -> [a]
reverseLeft'1 = foldl (\acc x -> x:acc) []

reverseLeft'2 :: [a] -> [a]
reverseLeft'2 = foldl (\acc x -> (:) x acc) []

reverseLeft'3 :: [a] -> [a]
reverseLeft'3 = foldl (\acc x -> flip (:) acc x) []

reverseLeft'4 :: [a] -> [a]
reverseLeft'4 = foldl (\acc -> flip (:) acc) []

reverseLeft'5 :: [a] -> [a]
reverseLeft'5 = foldl (flip (:)) []


--- 3.3 --
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x,y) <- zip xs ys]

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f [] [] = []
zipWith'' f (x:xs) (y:ys) = [f x y] ++ zipWith'' f xs ys


-- 3.4 --
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =   if x <= y
                    then x:y:ys
                    else y : insert x ys

isort :: Ord a => [a] -> [a]
isort l = foldr insert 0 l