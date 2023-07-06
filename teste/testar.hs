

dups :: [a]->[a]
dups [] = []
dups xs = dupsAux (zip xs [0 ..])


dupsAux ::(Integral b) =>[(a,b)] -> [a]
dupsAux [] = []
dupsAux ((x,y):xs)
    |(mod y 2) == 0 = x:x:dupsAux xs
    | otherwise = x:dupsAux xs


vogais = ['a','e','i','o','u']

transforma :: String -> String
transforma [] = []
transforma (x:xs)
    | elem x vogais = x:'p':x:transforma xs
    | otherwise = x:transforma xs


type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = [];



prodInterno :: Vector -> Vector -> Int
prodInterno [] l = 0
prodInterno v [] =0
prodInterno v l = sum (zipWith (*) v l)
--prodInterno (x:xs) (y:ys)= x*y + prodInterno xs ys


data Arv a = F | N a (Arv a) (Arv a) deriving(Show)
