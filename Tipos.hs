data LinkedList a = Vazia | No a (LinkedList a) deriving (Eq, Show)
data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)
--data Trem a = Vagao a ( Trem a ) | Vazio deriving Show

--Linked List
fromList :: [a] -> LinkedList a
fromList [] = Vazia
fromList list = No (head list) (fromList (tail list))

toList :: LinkedList a -> [a]
toList Vazia = []
toList (No x y) = x : toList y 

append :: a -> LinkedList a -> LinkedList a
append num (No x Vazia) = No x (No num Vazia)
append num (No x y) = No x (append num y)

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList (No x y) = fromList(reverse(toList (No x y)))


--Make Mobile
makeMobile :: [Int] -> Mobile
makeMobile [x] = Pendente x
makeMobile list = Barra (makeMobile (take x list)) (makeMobile (drop x list))
                  where x = length list `div` 2

--Trem
data Trem a = Vagao a ( Trem a ) | Vazio deriving Show
type Quantidade = Int
type Peso = Int
data Carga = SemCarga | Passageiro Quantidade | Mercadoria Peso deriving Show

--isPassageiro (Passageiro x) = x; 
numPassageiros (Vazio) = 0  
numPassageiros (Vagao SemCarga y) =  0 + numPassageiros y
numPassageiros (Vagao (Mercadoria x) y) = x + numPassageiros y
numPassageiros (Vagao (Passageiro x) y) =  x + numPassageiros y