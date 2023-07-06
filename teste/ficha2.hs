import Data.Char

--- 2.1)
-- a)
myand :: [Bool] ->Bool
myand [] = True
myand (x:xs)
    | x== False = False
    | otherwise = True && (myand xs)
-- bastava isto myand (x:xs) = x && myand xs

--b) 
myor :: [Bool] -> Bool
myor [] = False
myor (x:xs)= x || (myor xs)

-- c)
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs)

-- d)
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate 1 a = [a]
myreplicate n a = a:(myreplicate (n-1) a)

-- e)
(!!!) :: [a] -> Int -> a 
(!!!) [] _ = error "empty list!"
(!!!) l 0 = head l
(!!!) (x:xs) n 
    | n < 0 = error "Indice menor que zero"
    | otherwise = (!!!) (xs) (n-1)

-- f)
myelem :: Eq a => a -> [a] -> Bool
myelem a (x:xs)
    | a==x = True
    |otherwise = myelem a xs

--- 2.2)
intersperse :: a -> [a] -> [a]
intersperse a [] = []
intersperse a [x] = [x]
intersperse a (x:xs)= x:a:(intersperse a xs)

--- 2.3)

mdc :: Integer -> Integer -> Integer 
mdc a b 
    | b==0 = a
    | otherwise = mdc b ( a `mod` b)

--- 2.4)
-- a)
insert :: Ord a => a -> [a] -> [a] 
insert n [] = [n]
insert n (x:xs)
    | n<x = n:x:xs
    |otherwise = x:(insert n xs) 

-- b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- 2.5)
-- a)
myminimum :: Ord a => [a] -> a
myminimum [] = error "lista vazia"
myminimum [a] = a
myminimum (x:xs) 
    | x <= myminimum xs = x
    | otherwise = myminimum xs

--b)
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete a (x:xs)
    | a==x = xs
    | otherwise = x:(mydelete a xs)


--- 2.6)
somarquadrados :: (Num a) => [a] -> a
somarquadrados l = sum [x*x | x <- l]

--- 2.7)
-- a)
aprox1 :: Int -> Double
aprox1 n = sum [((-1)^u)/(fromIntegral(2*u+1)) | u <- [0 .. n]]

-- b)
aprox2 :: Int -> Double
aprox2 n = sum [((-1)^u)/fromIntegral((u+1)^2) | u <- [0 .. n]]


--- 2.8)
dotprod :: [Float] -> [Float] -> Float
dotprod [] [] =0
dotprod x y = sum [a*b | (a,b)<- zip x y]

--- 2.9)
divprop :: Integer -> [Integer]
divprop x = [i | i<-[1 .. x-1] , x `mod` i ==0]

--- 2.10)
perfeitos :: Integer -> [Integer ]
perfeitos n = [i | i <- [1 .. n-1] ,sum (divprop i)==i]

--- 2.11)
pitagoricos :: Integer -> [(Integer ,Integer ,Integer)]
pitagoricos n = [(x,y,z) | x<-[1 .. n], y<-[1 .. n], z<-[1 .. n], x*x+y*y==z*z]

--- 2.12)
primo :: Integer -> Bool
primo l = if ( length (divprop l)==1) then True else False


--2.13)
isMerssene :: Integer -> Bool
isMerssene n = primo n && not (null [i | i <-[0..n],(2^i - 1) == n])

mersennes :: [Integer]
mersennes = [i | i <-[1..], isMerssene i]

---2.14)
binom :: Int -> Int -> Int
binom n k =  fact n `div` ( fact k * fact (n -k))
    where fact n = product [1..n]

pascal :: Int -> [[Int]]
pascal n =  [[ binom i j | j <- [0..i]]  | i <- [0..n]]

--- 2.16)
myconcat16 :: [[a]] -> [a]
myconcat16 l = [x | u<-l , x<-u]

myreplicate16 :: Int -> a -> [a]
myreplicate16 n a = [a | i<-[1 .. n]] 

(@@@) :: (Integral b) => [a] -> b -> a
l @@@ n = head [x | (x,i) <- zip l [0 ..], i == n]


--- 2.17)
forte :: String -> Bool
forte l = (length l >= 8) && aaiuscula  && iinuscula && llgarismo 
    where 
        aaiuscula = or (map (isUpper) l)
        iinuscula = or (map (isLower) l)
        llgarismo = or (map (isDigit) l)
 

