import Data.List

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        part2 = filter (<0) [parse line [] | line <- contents]
    in show (
        -- Part 1: Parse each line, keep only positive numbers, then take the sum.
        sum $ filter (>0) [parse line [] | line <- contents],
        -- Part 2: Parse each line, keep only negative numbers, sort and then take the absolute value of the median.
        abs $ (sort part2) !! ((length part2) `div` 2)
    )

-- Takes a string and a stack, and recursively matches the open/close characters in the string.
-- Pushes open characters onto the stack, compares closing characters with the top of stack.
-- If the string is empty, folds the remaining stack and sums the points (but negatively, to separate it from part 1).
parse :: String -> [Char] -> Int
parse [] stack = foldl (\acc x -> (-5 * abs acc) - (points2 x)) 0 stack
parse (x:xs) []
    | x `elem` "([{<"   = parse xs [convert x]
    | otherwise         = points x
parse (x:xs) stack@(top:rest)
    | x `elem` "([{<"   = parse xs (convert x:stack)
    | x == top          = parse xs rest
    | otherwise         = points x

convert :: Char -> Char
convert '(' = ')'
convert '[' = ']'
convert '{' = '}'
convert '<' = '>'
convert x = error $ "Unable to convert " ++ [x]

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points x = error $ "Unable to points " ++ [x]

points2 :: Char -> Int
points2 ')' = 1
points2 ']' = 2
points2 '}' = 3
points2 '>' = 4
points2 x = error $ "Unable to points2 " ++ [x]
