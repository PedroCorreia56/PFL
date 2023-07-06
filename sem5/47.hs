inverte :: [a] -> [a]
inverte []=[]
inverte (x:xs) = inverte xs ++ [x]


inverter :: IO ()
inverter = do    
    x <- getLine
    let invertido= inverte x
    putStrLn invertido
            -- Existe uma função chamada reverse oh burro
            