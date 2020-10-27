gangorra p1 c1 p2 c2 = 
    if p1 * c1 == p2 * c2
        then 0
        else if p1 * c1 > p2 * c2
                then -1
                else 1

main = do
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine
    line4 <- getLine
    let p1 = read line1 :: Int
    let c1 = read line2 :: Int
    let p2 = read line3 :: Int
    let c2 = read line4 :: Int
    print $ gangorra p1 c1 p2 c2