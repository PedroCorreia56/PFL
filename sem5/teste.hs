remover :: [Int]->[Int]->[(Int,Int)] -> [Int]
remover [] _ _ = []
remover l [] _= l
remover (x:xs) (y:ys) (z:zs)
    | snd(z)==y = remover xs ys zs
    | otherwise = x:remover xs (y:ys) zs