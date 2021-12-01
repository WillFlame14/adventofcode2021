main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        nums = [read x :: Int | x <- contents]
    in show (increasing2 nums)

increasing :: [Int] -> Int
increasing [x] = 0
increasing (x1:x2:xs) = (if x1 < x2 then 1 else 0) + increasing (x2:xs)

increasing2 :: [Int] -> Int
increasing2 [x1, x2, x3] = 0
increasing2 (x1:x2:x3:x4:xs) = (if x1 < x4 then 1 else 0) + increasing2 (x2:x3:x4:xs)
