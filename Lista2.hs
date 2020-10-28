paridade list = odd size
                where size = length[x | x <- list, x == True]

rev num = read (reverse(show num)) :: Integer

delete' _ [] = []
delete' num list =
    if head list == num
        then tail list
        else head list : delete' num (tail list)

swap list i1 i2 = 
    if i1 < i2
        then take i1 list ++ list !! i2 : take (i2 - 1) (drop (i1 + 1) list) ++ list !! i1 : drop (i2 + 1) list
        else swap list i2 i1

nextPerm' [] list2 i = -1
nextPerm' list1 list2 i
    |list2 == head list1 = i
    |otherwise = nextPerm' (tail list1) list2 (i+1)
nextPerm list = (allPerms list) !! ((nextPerm' (allPerms list) list 0) + 1)

allPerms [] = [[]]
allPerms list = [x:y | x <- list, y <- (allPerms (filter (\a -> a /= x) list))] 

buscaBin' i [] num = -1
buscaBin' i list num
    | num == head list = i
    | otherwise = buscaBin' (i + 1) (tail list) num
buscaBin list num = buscaBin' 0 list num

primos [] = []
primos list = head list : primos [x | x <- list, x `mod` (head list) /= 0]
formTuples [] = []
formTuples list = (head xs, length xs) : formTuples ys
                where xs = filter (== minimum list) list
                      ys = filter (/= minimum list) list
factors' 1 list = []
factors' num list
    | num `mod` (head list) == 0 = head list : factors' (num `div` (head list)) list 
    | otherwise = factors' num (tail list)
factors num = formTuples (factors' num (primos [2..]))

listacc list = map (sum) [take x list | x <- [1 .. (length list)]]

maxsseq' [] = []
maxsseq' list = [take x list | x <- [1 .. (length list)]] ++ maxsseq' (tail list)
maxsseq list = head [x | x <- (maxsseq' list),  sum (x) == (maximum (map (sum) (maxsseq' list)))]
