menorDeDois num1 num2 = min num1 num2   --1. Menor de 2

menorDeTres num1 num2 num3 = min num1 (min num2 num3)    --2. Menor de 3

fatorial num = product[num, (num - 1)..1]    --3. Fatorial de um número

fibonacci num =     --4. n-esimo termo de fibonacci
  if num == 0
    then 0
    else if num == 1
      then 1
      else fibonacci (num - 1) + fibonacci (num - 2)

elemento num list =  list !! num    --5. n-ésimo termo de u

pertence num list = num `elem` list     --6. elemento pertencente a lista

total' list =         --7. Total de elementos sem length
  if null list
    then []
    else 1 : total' (tail list)

total list = sum(total' list)

maior list =                  --8. Maior número da lista
  if null (filter (\x -> x > head list) list)
    then head list
    else maior (filter (\x -> x > head list) list)

frequencia num list = length[x | x <- list, x == num]   --9. Total de números n em uma lista

unico num list = length[x | x <- list, x == num] == 1   --10. Verifica se um número aparece uma unica vez na lista

maioresQue num list = print $ [x | x <- list, x > num]    --11. Mostra todos os números da lista maiores que n

concat' list1 list2 = concat [list1,list2]    --12. Concatena duas listas

calda list = tail list    --13. Mostra todos os elementos da lista com exceção do primeiro

corpo list = init list    --14. Mostra todos os elementos da lista com exceção do ultimo

unique [] = []    --15. Remove as repetiçõs da lista
unique (e:list) =
  if not (null list)
    then e : unique (filter (/= e) list)
    else [e]

menores num list = [x | x <- list, y <- take num (qsort list), x == y]    --16. 3 menores elementos na ordem que aparecem 

--17. Lista [1, −1, 2, −2, 3, −3, · · · , num, −num]--
alter' num =
  if num == 0
    then []
    else num * (-1) : num : alter' (num - 1)

alter num = reverse(alter' num)
--17. Lista [1, −1, 2, −2, 3, −3, · · · , num, −num]--

reverso list = reverse list    --18. Inverte uma lista

--19. Pega os n primeiros elementos e transforma em tupla
divide' list num =
  if num == 0
    then []
    else head list : divide' (tail list) (num - 1)

divide list num = (divide' list num, drop num list)
--19. Pega os n primeiros elementos e transforma em tupla

intercal list1 list2 =                --20. Elementos de duas listas intercalados
  if length list1 + length list2 == 0
    then []
  else if null list1 && not (null list2)
    then list2
  else if null list2 && not (null list1)
    then list1
  else head list1 : head list2 : intercal (tail list1) (tail list2)

uniao list1 list2 = unique (list1 ++ list2)     --21. União de duas listas sem repetição

intersec list1 list2 = [x | x <- list1, y <- list2, x == y]   --22. Lista das chaves que list1 e list2 possuem em comum

sequencia num1 num2 = [num2..(num2 + num1 - 1)]   --23. Sequencia de n numeros(num1) começando em num 2

inserir num list =    --24. Insere número no local correto de uma lista ordenada
  if head list > num
    then num : list
    else head list : inserir num (tail list)

isSorted list = list == qsort(list)   --25. Diz se a lista é ordenada

qsort [] = []   --26. Quicksort
qsort list = qsort (filter (< head list) list) ++ filter (== head list) list ++ qsort (filter (> head list) list)

--27. Rotaciona o n números do começo para o final
rotEsq' num text =
  if num == 0
    then []
    else head text : rotEsq' (num - 1) (tail text)

rotEsq num text = drop (num `mod` (length text)) text ++ rotEsq' (num `mod` (length text)) text
--27. Rotaciona o n números do começo para o final

--28. Rotaciona o n números do começo para o final
rotDir' num text =
  if num == 0
    then []
    else last text : rotDir' (num - 1) (init text)

rotDir num text = reverse(rotDir' (num `mod` (length text)) text) ++ take (length text - num `mod` (length text)) text 
--28. Rotaciona o n números do começo para o final

upper [] = []       --29. Deixa o texto em caixa alta
upper text =
  if head text >= 'a' && head text <= 'z'
    then toEnum (fromEnum (head text) - 32) : upper (tail text)
    else if (head text >= 'A' && head text <= 'Z') || head text == ' '
      then head text : upper (tail text)
      else []

--30. A primeira letra de cada palavra em maiusculo
lowercase [] = []
lowercase text = 
  if head text >= 'A' && head text <= 'Z'
    then toEnum (fromEnum (head text) + 32) : lowercase (tail text)
    else head text : lowercase (tail text)
uppercase text = 
  if head text >= 'a' && head text <= 'z'
    then toEnum (fromEnum (head text) - 32) : lowercase (tail text)
    else head text : lowercase (tail text)
titulo' [] = []
titulo' list = uppercase (head [x | x <- words list]) : titulo' (unwords (tail [x | x <- words list]))
titulo list = unwords (titulo' list)
--30. A primeira letra de cada palavra em maiusculo

selec text list =     --31. Mostra os elemento de uma string por indice com base no elemento da lista
  if null list
    then []
    else text !! head list : selec text (tail list)

isPalind text = text == reverse text    --32. Diz se é palindromo

primo num = length (filter (\x -> num `mod` x == 0) [2..num]) == 1    --33. Diz se é primo

sdig num =        --34. Soma os dígitos de um número
  if num == 0
    then 0
    else num `mod` 10 + sdig (num `div` 10)

--36. Contar a quantidade de elementos em sequencia e guardar em sublistas individuais a quantidade de vezes que aparece e o valor
compac' list = 
  if length list == 1
    then [head list]
    else if head list == list !! 1
      then head list : compac' (tail list)
      else if head list /= list !! 1
        then [head list]
        else []

compac [] = []
compac list = 
  if length (compac' list) == 1
    then [head list] : compac (drop (length (compac' list)) list)
    else if length (compac' list) > 1
      then [length (compac' list), head list] : compac (drop (length (compac' list)) list)
      else []
--36. Contar a quantidade de elementos em sequencia e guardar em sublistas individuais a quantidade de vezes que aparece e o valor


splitints list = (filter odd list, filter even list)    --37. Lista com impares e pares da lista

perfeito num = length [x | x <- [1..num], x * x == num] == 1    --38. Retorna True se o número for um quadrado perfeito e False caso contrario

--39. Converte o numero num para base b
base' num b =
  if(num == 0)
    then []
    else [[x] | x <- ['0'..'9']++['A'..'Z']] !! (num `mod` b) ++ base' (num `div` b) b

base num b = reverse (base' num b)
--39. Converte o numero num para base b

partes' [] = [[]]    --40. Subconjuntos de um conjunto
partes' list = map (head list:) (partes' (tail list)) ++ partes' (tail list)
partes list = unique(partes' list)

 
{- bubblesort' [] = []
bubblesort' list = 
  if head list > list !! 1
    then case list of
      x:y:xs -> y:x:xs
  else bubblesort'(tail list)

bubblesort [] = []
bubblesort list =  -}