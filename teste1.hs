sublist num1 num2 list
  | (num1 >= 0) && (num2 >= 0) && (num2 <= (length list - 1)) = [(list !! num1)..(list !! num2-1)]
  | (num1 >= 0) && (num2 >= 0) && (num2 > (length list - 1)) = [(list !! num1)..(list !! (length list - 1))]
  | (num1 >= 0) && (num2 < 0) = [(list !! num1)..(list !! (length list - 1 + num2))]
  | (num1 > 0) && (num2 < 0) = [list !! num1..list !! ((length list) - 1 + num2)]
  | (num1 < 0) && (num2 < 0) && (num1 > num2) = [(list !! (num1*(-1)))..(list !! (num2*(-1))-1)]
  | (num1 < 0) && (num2 < 0) && (num2 > num1) = [(list !! (num2*(-1)))..(list !! (num1*(-1))-1)]
  | otherwise = []  

main = do
    begin <- readLn :: IO Int
    end   <- readLn :: IO Int
    line  <- getLine
    print $ sublist begin end [read x :: Int | x <- words line]