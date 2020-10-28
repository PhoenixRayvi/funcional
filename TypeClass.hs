import Numeric(showFFloat)

--Q1 - Complex
data Complex = Complex { real :: Float
                        ,img :: Float
                        }

instance Show Complex where
    show (Complex x y)= showFFloat (Just 3) x "" ++ " + " ++ showFFloat (Just 3) y "i"

instance Num Complex where
    (+) (Complex a b) (Complex c d) = Complex (a + c) (b + d)
    (-) (Complex a b) (Complex c d) = Complex (a - c) (b - d)
    (*) (Complex a b) (Complex c d) = (Complex ((a*c)-(b*d)) ((a*d)+(b*c)))
    negate (Complex a b) = Complex (-a) (-b)
    abs (Complex a b) = (Complex (sqrt (a^2 + b^2)) 0)
    signum (Complex a b) = (Complex (sqrt a - signum a) (sqrt b - signum b))
    fromInteger n   = Complex (fromIntegral n::Float) 0

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
{-
data Pessoa = Pessoa { nome :: String
            , idade :: Int
            , salario :: Float 
            }

data Criterio = ByNome | ByIdade | BySalario

instance Show Pessoa where
    show (Pessoa n i s) = n ++ " tem " ++ show i ++ " anos e ganha de salario " ++ show s

sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa list ByNome = filter (< ((head list).idade)) list
-}
