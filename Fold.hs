concatenaFold list = foldr (++) [] list

paridadeFold list
    |(foldr(\x y -> if x == True then 1 + y else y) 0 list) `mod` 2 == 0 = True
    |otherwise = False

inverteFold list = foldr (\x y -> y ++ [x]) [] list

duplicarFold list = foldr (\x y -> if x `elem` "aeiouAEIOU" then x:x:y else x:y) [] list

filtraAplica f p = foldr(\x y -> if p x then f x : y else y) []

mapFold f = foldr (\x y -> f x : y) []

removeLista list1 list2 = foldr (\x y -> if x `elem` list1 then y else x : y) [] list2

acertosFold list1 list2 = foldr (\x y -> if list1 !! x == list2 !! x then y + 1 else y) 0 [0..(length list1) - 1]

descompactaFold list = foldr (\(a,b) (c,d) -> (a:c, b:d) ) ([], []) list

