paridade list = odd size
                where size = length[x | x <- list, x == True]

rev num = read (reverse(show num)) :: Integer

delete' _ [] = []
delete' num list =
    if head list == num
        then tail list
        else head list : delete' num (tail list)

listPrimes [] = []
listPrimes list = 
    if length [head list | x <- [2..100], head list `mod` x == 0] == 1
        then head list : listPrimes (tail list)
        else listPrimes (tail list)

biggerNum [x] = x
biggerNum list = 
    if head list > list !! 1
        then biggerNum (head list : drop 2 list)
        else biggerNum (tail list)

swap list i1 i2 = 
    if i1 < i2
        then take i1 list ++ list !! i2 : take (i2 - 1) (drop (i1 + 1) list) ++ list !! i1 : drop (i2 + 1) list
        else swap list i2 i1
--nextPerm list = 
--swap list num1 num2 = take num1 list ++ list !! num2 : drop (num1 + 1) list