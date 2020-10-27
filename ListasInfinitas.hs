kolakoski = [1,2,2] ++ concat [replicate x y |(x, y) <- zip (drop 2 kolakoski) (cycle [1,2])]

collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | otherwise = x : collatz ((x * 3) + 1)

fechoKleene list = [] : [x ++ [y] | x <-fechoKleene list, y<-list]

primos list = head list : primos [x | x <- tail list, x `mod` (head list) /= 0]

primosPalindromos = [x | x <- primos [2..], show x == reverse (show x)]

primosGemeos' list
    | head (primos list) + 2 == (primos list) !! 1 = (head (primos list), (primos list) !! 1) : primosGemeos' (tail (primos list))
    | otherwise = primosGemeos' (tail (primos list))
primosGemeos = primosGemeos' [2..]
{-
mescla3 list1 list2 list3 =  newList1 ++ newList2 -- ++ [head list3] ++ mescla3 (take (length newList1) list1) (take (length newList2) list2) (tail list3)
                            where newList1 = filter (< head list3) list1
                                  newList2 = filter (< head list3) list2

hamming = mescla3 [2*x | x <- [1..]] [3*x | x <- [1..]] [5*x | x <- [1..]]
-}
