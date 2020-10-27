kolakoski = [1,2,2] ++ concat [replicate x y |(x, y) <- zip (drop 2 kolakoski) (cycle [1,2])]

merge list1 list2 | head list1 < head list2 = head list1 : merge (tail list1) list2
                        | head list2 < head list1 = head list2 : merge list1 (tail list2)
                        | otherwise = head list1 : merge (tail list1) (tail list2)

hamming = 1 : merge (merge (map (*2) hamming) (map (*3) hamming)) (map (*5) hamming)

collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | otherwise = x : collatz ((x * 3) + 1)

fechoKleene list = [] : [x ++ [y] | x <-fechoKleene list, y<-list]

goldbach num = [head [(x,y,z) |  y <- primos [2..num], z <- primos [2..num], y + z == x] | x <- [4,6..num]]

primos [] = []
primos list = head list : primos [x | x <- tail list, x `mod` (head list) /= 0]

primosPalindromos = [x | x <- primos [2..], show x == reverse (show x)]

primosGemeos' list
    | head (primos list) + 2 == (primos list) !! 1 = (head (primos list), (primos list) !! 1) : primosGemeos' (tail (primos list))
    | otherwise = primosGemeos' (tail (primos list))
primosGemeos = primosGemeos' [2..]
