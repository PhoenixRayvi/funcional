rmMiolo list = putStrLn $ "" ++ show (list !! 0) ++ " " ++ show(list !! (length (list) - 1))

intervalo list pointA pointB = 
    if pointB > pointA
        then putStrLn $ show [x | x <- list, x > pointA, x < pointB]
        else putStrLn $ show [x | x <- list, x > pointB, x < pointA]

inter2 list pointA pointB = putStrLn $ show [x | x <- list, x >= pointA, x <= length (list) + pointB]
