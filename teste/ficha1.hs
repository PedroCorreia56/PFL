--- 1.4
-- a)
mylast1 :: [a] -> a
mylast1 l = head (reverse l)

-- b)
mylast2 :: [a] -> a 
mylast2 l =head ( drop (length l -1) l)


-- 1.5)

-- a) 
binom :: Integer -> Integer -> Integer
binom n k = (product [1 .. n]) `div` ( (product [1 .. k]) * (product [1 .. (n-k)]) )


-- 1.6)

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((-b + sqrt(cenas))/(2*a),(-b - sqrt(cenas))/(2*a))
    where cenas = b*b-4*a*c

-- 1.7) 
-- a) ['a','b','c'] -> [Char] correto
-- b) ('a','b','c') -> (Char,Char,Char)  correto
-- c) [(False,'0'),(True,'1')] -> [(Bool,Char)] correto 
-- d) ([False,True],['0','1'])->([Bool],[Char]) correto
-- e) [tail,init,reverse] -> [funcoes] [[a]->a]  correto
-- f) [id,not] -> [a->a] errado [Bool->Bool]

-- 1.8)
-- a ) 
--segundo :: [a] -> a correto
segundo xs = head (tail xs)

-- b)
-- trocar :: (a,b) -> (b,a) correto
trocar (x, y) = (y, x)

-- c) 
-- par :: a -> b -> (a,b) correto
par x y = (x, y)

-- d)
-- dobro :: (Num a)=> a -> a correto
dobro x = 2 * x

-- e)
-- metade :: (Fractional a)=> a -> a correto
metade x = x/2

-- f)
--  minuscula :: (Char a ) => a -> Bool correto
minuscula x = x >= 'a' && x <= 'z'

-- g)
-- intervalo :: (Ord x,Ord a ,Ord b) => x-> a -> b -> Bool  podia apenas ser intervalo :: Ord a => a -> a -> a -> Bool
intervalo x a b = x >= a && x <= b

-- h)
--palindromo :: [a] -> Bool , incompleto palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs 

-- twice :: (a->b)-> a -> b twice :: (t -> t) -> t -> t
twice f x = f (f x)


--- 1.9)
classifica :: Int -> String
classifica x 
    | x>=0 && x <= 9 = "reprovado"
    | x>=10 && x<=12 =  "suficiente"
    |  x>=13 && x<=15 =  "bom"
    | x>=16 && x<=18 =  "muito bom"
    |  x>=19 && x<=20 =  "muito bom com distincao"
    | otherwise = "Numero não disponivel"


--- 1.10)

imc :: Float -> Float -> String
imc peso altura
    | im>=0 && im < 18.5= "baixo peso"
    | im>=18.5 && im< 25 =  "peso normal"
    |  im>=25  && im< 30 =  "excesso de peso"
    | im>=30 =  "obeso"
    | otherwise = "Erro"
        where im=peso/(altura*altura)


--- 1.11)

max3 :: (Ord a)=> a -> a-> a -> a
max3 a b c 
    | a>=b && a >=c = a
    | b>=a && b >=c = b
    |otherwise = c

min3 :: (Ord a)=> a -> a-> a -> a
min3 a b c 
    | a<=b && a <=c = a
    | b<=a && b <=c = b
    |otherwise = c

--- 1.12)
xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor a b = True

-- 1.13)
mysafetail_pattern :: [a] -> [a]
mysafetail_pattern [] = []
mysafetail_pattern l = tail l

mysafetail_guard :: [a] -> [a]
mysafetail_guard l
    | null l = []
    | otherwise = tail l

safetail_if ::[a] -> [a]
safetail_if xs = if null xs then [] else tail xs

-- 1.14)
curta :: [a] -> Bool
curta l 
    | length l <=2 = True
    |otherwise = False