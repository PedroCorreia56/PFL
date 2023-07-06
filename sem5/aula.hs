import Permutations (
    myPermutations,myPermutations2
    )-- se omitir a função dá import a tudo
--IMPORTANTE PARA O TESTE

-- IO pertece a Monads-> is a value, whatever we want, wrapped inside a context
-- for example: Maybe Int -> the functions can return: Just 2 or Nothing
-- getLine receives the string we give and stores it inside an IO
-- Lists are actually a Monad, [] represents non-determinism
-- Other Monads: Either -> like Either Int Bool (we either receive an Int or a boolean)

inverteInput :: IO () -- funçao que vai dar print a algo
inverteInput = do
    str <- getLine
    putStr $ reverse str

inverteInput2 :: IO () -- funçao que vai dar print a algo
inverteInput2 = do
    str <- getLine
    if(null str) then 
        return () -- parenteses necessários
    else do
        putStr $ reverse str
        inverteInput2

-- ex 4.9

elefantes :: Int -> IO () -- IO is empty because is only printing, not receiving a value
elefantes n = elefantesAux 2 n 

-- 1º int é indice 2º é o numero final de elefantes
elefantesAux :: Int -> Int -> IO ()
elefantesAux i n 
    | i < n = do 
        putStrLn $ "Se " ++ (show i) ++ " incomodam muita gente," -- cant do ++i++ because i is an int
        putStrLn $ (show (i+1)) ++" elefantes incomodam muito mais!" 
        elefantesAux (i+1) n
    | otherwise = return ()


-- Quando no exame pedem higher order, não fazer recursão ou listas de compreensão

-- LI-39
permIO :: IO ()
permIO = do
    putStrLn "INput max int: "
    input <- getLine
    let num = read input :: Int
    if(num>=1) then
        putStrLn $ show $ myPermutations2 [1 .. num]
    else do 
        putStrLn "Int tem de ser maior que 0"
        permIO