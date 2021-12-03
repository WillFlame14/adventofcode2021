import Data.Char

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        sums = find_sum contents
    in show (
        (product (map binaryToDec (gammaEpsilon sums))),    -- part 1
        (binaryToDec (oxygen (contents, 0)) * binaryToDec (co2 (contents, 0)))  -- part 2
    )

-- Given a bunch of strings of binary numbers, sums each digit individually and returns an array of sums
find_sum :: [String] -> [Int]
find_sum [] = replicate 12 0
find_sum (x:xs) = zipWith (+) (map digitToInt x) (find_sum xs)

-- Given the sums array, generate the gamma and epsilon strings
gammaEpsilon :: [Int] -> [String]
gammaEpsilon [] = ["", ""]
gammaEpsilon (num:xs)
    | num > 500 = zipWith (:) ['1', '0'] rest
    | otherwise = zipWith (:) ['0', '1'] rest
    where rest = gammaEpsilon xs

-- Given the current list of strings and an index, return the oxygen string
oxygen :: ([String], Int) -> String
oxygen ([x], _) = x
oxygen (xs, index) =
    let sums = find_sum xs
        most_common = (if sums !! index >= (length xs + 1) `div` 2 then '1' else '0')
    in oxygen ([x | x <- xs, x !! index == most_common], index + 1)

-- Given the current list of strings and an index, return the co2 string.
-- I can't find a good way to combine this with the oxygen function since the list of strings is different
co2 :: ([String], Int) -> String
co2 ([x], _) = x;
co2 (xs, index) =
    let sums = find_sum xs
        least_common = (if sums !! index >= (length xs + 1) `div` 2 then '0' else '1')
    in co2 ([x | x <- xs, x !! index == least_common], index + 1)

-- Converts a string of a binary number into decimal
binaryToDec :: String -> Int
binaryToDec x =
    let mults = [2^x | x <- [(length x - 1), (length x - 2)..0]]
        in sum (zipWith (*) (map digitToInt x) mults)