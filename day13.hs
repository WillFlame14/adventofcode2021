import Data.Char
import Data.List
import qualified Data.Set as Set

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        (dots_raw, inst_raw) = break (== "") contents
        dots = map (\x -> map read (wordsWhen (== ',') x):: [Int]) dots_raw
        dot_set = foldl (\set [x, y] -> Set.insert (hash (x, y)) set) Set.empty dots
        max_dim = foldl (\(max_x, max_y) [x, y] -> (if x > max_x then x else max_x, if y > max_y then y else max_y)) (0, 0) dots
        paper = [[if Set.member (hash (x, y)) dot_set then '#' else '.' | x <- [0..(fst max_dim)]] | y <- [0..(snd max_dim)]]
        inst = map (\x -> (words x) !! 2) (drop 1 inst_raw)
        inst_parts = map (\x -> let [dir, dist] = wordsWhen (== '=') x in (head dir, read dist :: Int)) inst
    in show (
        sum [length $ filter (== '#') row | row <- fold (inst_parts !! 0) paper],
        foldl (\new_paper new_inst -> fold new_inst new_paper) paper inst_parts
    )

-- Splits a string on a predicate.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- Performs a fold on the paper given an instruction.
-- If we need to do an x-fold, we simply change the rows into columns and then do a y-fold.
fold :: (Char, Int) -> [[Char]] -> [[Char]]
fold (dir, dist) paper
    | dir == 'y'    = fold_y dist paper
    | otherwise     = transpose $ fold_y dist (transpose paper)

-- Folds the paper on the specified y-value. Goes from top to y and merges the reflected row.
fold_y :: Int -> [[Char]] -> [[Char]]
fold_y y paper = [merge (get_row row paper) (get_row (2*y - row) paper) | row <- [0..(y - 1)]]

-- Merges two strings together.
merge :: String -> String -> String
merge x "" = x
merge x y = reverse . foldl (\acc (x1, y1) -> (if x1 == '.' && y1 == '.' then '.' else '#'):acc) "" $ zip x y

-- Safe version of array access. Actual bounds checking is too gross.
get_row :: Int -> [[Char]] -> String
get_row index paper
    | index >= length paper = ""
    | otherwise             = paper !! index

-- Hashing function for the set of dots at the very beginning, which makes generating the initial table *significantly* faster.
hash :: (Int, Int) -> Int
hash (x, y) = 10000*x + y
