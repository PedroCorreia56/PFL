curta :: [a] -> Bool
curta l = if (length l <= 2) then True else False

curta' :: [a] -> Bool
curta' l = length l <= 2

curta'' [] = True
curta'' [_] = True
curta'' [_, _] = True
curta'' _ = False



five :: Integer -> [Char]
five n = if(n == 5) then "five" else "not five"

five' :: Integer -> [Char]
five' n 
    | n == 5 = "five"
    | n == 4 = "four"
    | otherwise = "not five"

five'' 5 = "five"
five'' 4 = "four"
five'' _ = "not five"

five''' n = case n of   5 -> "five"
                        4 -> "four"
                        _ -> "not five"



last' :: [a] -> a 
last' l = head (reverse l)

last'' :: [a] -> a 
last'' l = head (drop (length l - 1) l)

init' :: [a] -> [a]
init' l = take (length l - 1) l

init'' :: [a] -> [a]
init'' l = reverse (tail (reverse l))



metades :: [a] -> ([a], [a])
metades l = (take halflen l, drop halflen l)
    where halflen = (length l) `div` 2


