main = interact sol

sol :: String -> String
sol input =
    let contents = map read (wordsWhen (==',') input) :: [Int]
    in show (
        solve contents,
        solve2 contents
    )

-- Splits a string on a predicate.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- Finds the total fuel distance given a target and the list of starting values. (Part 1)
find_dist :: Int -> [Int] -> Int
find_dist target xs = sum [abs (x - target) | x <- xs]

-- Finds the total fuel distance given a target and the list of starting values. (Part 2)
find_dist2 :: Int -> [Int] -> Int
find_dist2 target xs = sum [dist !! (abs (x - target)) | x <- xs]

solve :: [Int] -> Int
solve xs = minimum [find_dist x xs | x <- [1..(maximum xs)]]

solve2 :: [Int] -> Int
solve2 xs = minimum [find_dist2 x xs | x <- [1..(maximum xs)]]


-- I generated the list of triangular numbers myself because... I'm not sure why, actually.
nums = [0..2000]

nums2 [] _ = []
nums2 (x:xs) save = (save + x):(nums2 xs (save + x))

dist = nums2 nums 0


