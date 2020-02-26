qtdIgual x y z =
    if x == y && x == z
        then 3
    else if x /= y && x /= z && y /= z
        then 0
        else 2

main = do
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine
    let num1 = read line1 :: Int
    let num2 = read line2 :: Int
    let num3 = read line3 :: Int
    print $ qtdIgual num1 num2 num3;