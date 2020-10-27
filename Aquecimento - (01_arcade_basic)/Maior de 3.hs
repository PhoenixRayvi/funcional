max3 x y z = max x (max y z)

main = do
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine
    let num1 = read line1 :: Int
    let num2 = read line2 :: Int
    let num3 = read line3 :: Int
    print $ max3 num1 num2 num3