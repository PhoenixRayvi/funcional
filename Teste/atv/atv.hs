conv list = unwords (map show list)

rot list n = drop (n `mod` (length list)) list ++ take (n `mod` (length list)) list