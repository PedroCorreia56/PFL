data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

type Pair a b = (a, b)


-- 4.1 --
sumArv :: (Num a) => Arv a -> a
sumArv Vazia = 0
sumArv (No x l r) = x + (sumArv l) + (sumArv r)

myArv :: Arv Int
myArv = (No 3(No 2(No 1 Vazia Vazia) Vazia) (No 4 Vazia Vazia))

-- 4.4 --
mapArv :: (a -> b) -> Arv a -> Arv b 
mapArv _ Vazia = Vazia
mapArv f (No x l r) = No (f x) (mapArv f l) (mapArv f r)

f :: (Num a) => a -> a
f a = a+a

-- Infinte List Exercise --
-- use ' :set +s ' to measure run time, run it before the call you want to measure

calcPi1 :: (Floating b, Enum b) => Int -> Int -> b 

calcPi1 1 n = sum [fromIntegral(4*(-1)^i) / fromIntegral(1 + 2*i) | i <- [0 .. n-1]]

calcPi1 2 n = sum $ take n $ zipWith (/) (cycle [4, -4]) [fromIntegral(1+2*i) | i <- [0 .. n-1]]


calcPi2 :: (Floating b, Enum b) => Int -> Int -> b 

calcPi2 1 n = 3 + sum [fromIntegral(4*(-1)^i) / fromIntegral(product [2*(i+1) .. 2*(i+1) + 2]) | i <- [0 .. n-1]]

calcPi2 2 n = 3 + (sum $ take (n-1) $ zipWith (/) (cycle[4,-4]) [fromIntegral(product [2*(i+1) .. 2*(i+1) + 2]) | i <- [0 ..]])

calcPi2 3 n = 3 + (sum $ take (n-1) $ zipWith (/) (cycle[4,-4]) (zipWith (*) [2,4 ..] (zipWith (*) [3,5 ..] [4,6 ..])))

