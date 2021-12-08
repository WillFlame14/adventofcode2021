import Data.List

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        perform (x, y) =
            let key = solve $ words x
                digits = parse key (words (drop 2 y))
                in foldl (\acc x -> 10 * acc + x) 0 digits      -- transform the array of digits into a number
    in show (
        sum [uniq (drop 2 (dropWhile (/= '|') x)) | x <- contents], -- part 1
        sum [perform $ break (=='|') x | x <- contents]             -- part 2
    )

-- Given a string, finds the number of words that are 2, 3, 4 or 7 letters long
uniq :: String -> Int
uniq x =
    let parts = words x
        in length $ filter (\x -> length x `elem` [2,3,4,7]) parts

-- 7 - 1 is a
-- ag are shared in all the 5-6 letter words (find g)
-- adg are shared in the 5-letter words (find d)
-- 4 - 1 is bd (find b)
-- abfg are shared in the 6-letter words (find f)
-- 1 is cf (find c)
-- e is the last one

-- Given a list of unique strings, figures out what each number looks like.
-- Each number's letters are in alphabetical order to allow for easier comparison.
solve :: [String] -> [String]
solve parts =
    let seven = head $ filter (\x -> length x == 3) parts
        one = head $ filter (\x -> length x == 2) parts
        a = head $ filter (\x -> not (x `elem` one)) seven
        fivesix = filter (\x -> length x `elem` [5,6]) parts
        g = head $ filter (\x -> (x /= a) && (and [x `elem` word | word <- fivesix])) "abcdefg"
        five_lens = filter (\x -> length x == 5) parts
        d = head $ filter (\x -> (x /= a) && (x /= g) && (and [x `elem` word | word <- five_lens])) "abcdefg"
        four = head $ filter (\x -> length x == 4) parts
        b = head $ filter (\x -> x /= d && not (x `elem` one)) four
        six_lens = filter (\x -> length x == 6) parts
        f = head $ filter (\x -> not (x `elem` [a,b,g]) && (and [x `elem` word | word <- six_lens])) "abcdefg"
        c = head $ filter (\x -> x /= f) one
        e = head $ filter (\x -> not (x `elem` [a,b,c,d,f,g])) "abcdefg"
        zero_n = sort [a,b,c,e,f,g]
        one_n = sort [c,f]
        two_n = sort [a,c,d,e,g]
        three_n = sort [a,c,d,f,g]
        four_n = sort [b,c,d,f]
        five_n = sort [a,b,d,f,g]
        six_n = sort [a,b,d,e,f,g]
        seven_n = sort [a,c,f]
        eight_n = sort [a,b,c,d,e,f,g]
        nine_n = sort [a,b,c,d,f,g]
        in [zero_n, one_n, two_n, three_n, four_n, five_n, six_n, seven_n, eight_n, nine_n]

-- Given a key of numbers and a set of "words", returns an array of the numbers represented
parse :: [String] -> [String] -> [Int]
parse _ [] = []
parse key (word:rest) =
    let sorted = sort word
        key_index = zip key [0..9]
        (_, num) = head $ filter (\(x, index) -> x == sorted) key_index
        in num:(parse key rest)
