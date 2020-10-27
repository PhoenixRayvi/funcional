paridade list = odd size
                where size = length[x | x <- list, x == True]

rev 0 = 0
rev num = num `mod` 10 + rev (num `div` 10)