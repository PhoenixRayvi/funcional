data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)
data MultiSet a = MultiSet [(a,Int)] deriving (Show)

insertArvore num Vazia = No num Vazia Vazia
insertArvore num (No x esq dir)
    | num <= x = No x (insertArvore num esq) dir
    | otherwise = No x esq (insertArvore num dir)

{- |Testes arvore
arv1 = Vazia
arv2 = No 3 arv1 arv1
arv3 = No 5 arv1 arv1
arv4 = No 4 arv2 arv3
arv5 = No 4 arv3 arv2
arv6 = insertArvore 8 arv4
-}

--MultiSet questions
makeMultiSet' [] = []
makeMultiSet' list = (minimum list, length (filter (==minimum list) list)) : makeMultiSet' (filter (/=minimum list) list)
makeMultiSet list = MultiSet (makeMultiSet' list)

insere num (MultiSet list)
    |xs == list = MultiSet ([(x,y)| (x,y) <- list, x < num] ++ [(num, 1)] ++ [(x,y)| (x,y) <- list, x > num])
    |otherwise = MultiSet xs
    where xs = [if num == x then (x,y + 1) else (x,y)| (x,y) <- list]

delete num ocor (MultiSet list) = MultiSet (filter ((>0).snd) [if num == x then (x, (y - ocor)) else (x, y)| (x,y) <- list])