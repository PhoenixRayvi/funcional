soma x y = x + y

main = do
    line1 <- getLine
    line2 <- getLine
    let num1 = read line1 :: Int
    let num2 = read line2 :: Int
    print $ soma num1 num2