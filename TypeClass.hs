--Q2 - Date
data Mes = Janeiro 
            | Fevereiro 
            | Marco 
            | Abril 
            | Maio 
            | Junho 
            | Julho 
            | Agosto 
            | Setembro 
            | Outubro
            | Novembro
            | Dezembro
            deriving (Show, Ord, Eq, Enum) 

data Date = MkDate { dia :: Int 
            , mes :: Mes    
            , ano :: Int   
            }

instance Show Date where
    show (MkDate d m a) = show d ++ " de " ++ show m ++ " de " ++ show a

instance Eq Date where
    (==) (MkDate d1 m1 a1) (MkDate d2 m2 a2) = (d1 == d2) && (m1 == m2) && (a1 == a2)

instance Ord Date where
    (<=) (MkDate d1 m1 a1) (MkDate d2 m2 a2) | a1 /= a2 = if a1 < a2 then True else False
                                             | m1 /= m2 = if m1 < m2 then True else False
                                             | d1 /= d2 = if d1 < d2 then True else False
                                             | otherwise = True


--Q5

data Pessoa = Pessoa { nome :: String
            , idade :: Int
            , salario :: Float 
            }

data Criterio = ByNome | ByIdade | BySalario

instance Show Pessoa where
    show (Pessoa n i s) = n ++ " tem " ++ show i ++ " anos e ganha de salario " ++ show s

sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa list ByNome = filter (< ((head list).idade)) list
