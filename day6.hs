import Data.Char

main = interact sol

sol :: String -> String
sol input =
    let contents = parse input
        iterate_helper (x, y) = (iterate3 x y, 0)
    in show (
        length ((iterate iterate2 contents) !! 256),                            -- part 1
        sum (fst (iterate iterate_helper ((frequency contents), 0) !! 256))     -- part 2
    )

-- Reads "1,3,2,4" into [1,3,2,4]
parse :: String -> [Int]
parse [] = []
parse (x:xs) = if x /= ',' then (digitToInt x):parse xs else parse xs

-- Performs one iteration of lanternfish simulation on a list of lanternfish life stages.
iterate2 :: [Int] -> [Int]
iterate2 [] = []
iterate2 (x:xs)
    | x == 0    = 8:6:(iterate2 xs)     -- Prepend the new lanternfish since prepending is fast and order doesn't matter
    | otherwise = (x - 1):(iterate2 xs)

-- Processes an input list of numbers into a frequency table from 0 to 8
frequency :: [Int] -> [Int]
frequency xs = 0:[(length (filter (== a) xs)) | a <- [1..8]]

-- Performs one iteration of lanternfish simulation on a frequency table of lanternfish life stages.
iterate3 :: [Int] -> Int -> [Int]
iterate3 [x] save = [save]
iterate3 all@(x:x1:xs) save
    | length all == 9   = x1:(iterate3 (x1:xs) x)               -- Save the number of lanternfish that are spawning new lanternfish
    | length all == 3   = (x1 + save):(iterate3 (x1:xs) save)   -- All the lanternfish that were at 0 are now at 6
    | otherwise         = x1:(iterate3 (x1:xs) save)