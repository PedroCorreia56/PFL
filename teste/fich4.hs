
data Arv a = Vazia | No a (Arv a) (Arv a)
 
 -- 4.1)
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x Vazia Vazia)=x -- talvez não seja preciso
sumArv (No x esq dir)=sumArv esq + x + sumArv dir


--- 4.2)

listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar dir ++ [x] ++ listar esq


--- 4.3)

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x _ _ ) = [x]
nivel n (No _ esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

-- 4.4) idk what to do in this one

construir :: [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir xs') (construir xs'')
    where n = length xs `div` 2 -- ponto médio
          xs' = take n xs -- partir a lista
          x:xs'' = drop n xs

-- 4.5)
mapArv :: (a -> b) ->Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No x esq dir)= No (f x) (mapArv f esq) (mapArv f dir)


-- 4.6 )
-- a)
maisDir :: Arv a -> a
maisDir (No x _ Vazia) = x
maisDir (No _ _ dir) = maisDir dir


--- 4.7)
inverte :: IO()
inverte = do
            putStrLn "Insira uma string:"
            line <- getLine 
            putStrLn ("String invertida= " ++ (reverse line))
            inverte