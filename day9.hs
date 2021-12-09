import Data.List
import Data.Char
import qualified Data.Set as Set

main = interact sol

sol :: String -> String
sol input =
    let rows = lines input
        cols = transpose rows
        lows = [(x, y) | x <- [0..(length cols - 1)], y <- [0..(length rows - 1)], low x (rows !! y), low y (cols !! x)]
    in show (
        -- Part 1: For each of the low points, find the corresponding value on the grid and add 1. Then take their sum.
        sum [succ . digitToInt $ (rows !! y) !! x | (x, y) <- lows],
        -- Part 2: For each of the low points, explore to find the size of the basin. Then find the 3 largest basins and take their product.
        product . take 3 . reverse $ sort [explore (x, y) rows [(-1, -1)] $ Set.fromList [hash (x, y)] | (x, y) <- lows]
    )

-- Given an index and the "row" it's in, checks if the index is lower than the surrounding points.
-- This is called with the column (under the alias "row") in order to check columns as well.
low :: Int -> String -> Bool
low index rows =
    let curr_value = rows !! index
        left_valid  = index == 0 || curr_value < rows !! (index - 1)
        right_valid = index == length rows - 1 || curr_value < rows !! (index + 1)
        in left_valid && right_valid

-- Given the current point, the grid, a stack of points to visit, and a visited set, performs DFS on the grid to find the size of a basin.
-- At a point, we check the 4 directions and add all valid new points to the stack, making sure to add the current point to the visited set.
-- I chose a stack since it was the easiest to write a termination condition for (i.e. I used a dummy value at the bottom of the stack).
-- Note that we only need to check that a point isn't a 9 for it to be valid, not that it's higher than the current point.
-- This is because of the guarantee that all points only belong to 1 basin.
explore :: (Int, Int) -> [String] -> [(Int, Int)] -> Set.Set Int -> Int
explore (-1, -1) _ _ visited = Set.size visited
explore (x, y) rows stack visited =
    let possible = [(x + i, y + j) | i <- [-1,0,1], j <- [-1,0,1], abs(i + j) == 1, (x + i) >= 0, (x + i) < 100, (y + j) >= 0, (y + j) < 100]
        filtered_visited = filter (\x -> not (Set.member (hash x) visited)) possible
        to_visit = filter (\(x1, y1) -> ((rows !! y1) !! x1) /= '9') filtered_visited
        new_stack = foldl (\acc x -> x:acc) stack to_visit
        in explore (head new_queue) rows (tail new_stack) (Set.insert (hash (x, y)) visited)

-- Hashing function used for the visited set
hash :: (Int, Int) -> Int
hash (x, y) = 100 * x + y
