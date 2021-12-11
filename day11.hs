import Data.Char
import Data.List
import Data.Maybe(fromJust)
import qualified Data.Set as Set

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        grid = [map digitToInt line :: [Int] | line <- contents]
        -- Takes a grid, increments it, then calls resolve while maintaining the total number of flashes so far
        helper1 (grid, total_flashes) =
            let (new_grid, curr_flashes) = resolve (increment grid) (Set.fromList [])
                in (new_grid, total_flashes + curr_flashes)
        -- Just keep calling resolve, we don't need to keep track of the total number of flashes
        helper2 (grid, _) = resolve (increment grid) (Set.fromList [])
    in show (
        -- Part 1: Iterate helper1 100 times, then get the total number of flashes
        snd ((iterate helper1 (grid, 0)) !! 100),
        -- Part 2: Iterate helper2 infinitely until there are 100 flashes at once
        fromJust . findIndex (\(_, flashes) -> flashes == 100) $ iterate helper2 (grid, 0)
    )

-- Adds 1 to all elements in a grid
increment :: [[Int]] -> [[Int]]
increment xs = [map succ x | x <- xs]

-- Given a grid and a point (that is flashing), adds 1 to all the nearby points.
flash :: [[Int]] -> (Int, Int) -> [[Int]]
flash grid (x, y) =
    let flashed = [hash (x + i, y + j) | i <- [-1,0,1], j <- [-1,0,1], abs i + abs j /= 0, (x + i) >= 0, (x + i) < 10, (y + j) >= 0, (y + j) < 10]
        in [[(grid !! row) !! col + (if (hash (row, col)) `elem` flashed then 1 else 0) | col <- [0..9]] | row <- [0..9]]

-- Given a grid and a set of previously flashed points, returns a list of new points that should be flashed.
find_flashes :: [[Int]] -> Set.Set Int -> [(Int, Int)]
find_flashes grid flashed = filter (\(x, y) -> (grid !! x) !! y > 9 && not (Set.member (hash (x, y)) flashed)) [(x, y) | x <- [0..9], y <- [0..9]]

-- Given a grid and a list of points to flash, flashes all the points.
perform_flashes :: [[Int]] -> [(Int, Int)] -> [[Int]]
perform_flashes grid [] = grid
perform_flashes grid (flashing:to_flash) = perform_flashes (flash grid flashing) to_flash

-- Given a grid and a set of flashed points, determines if there are more points to flash and if so, flashes them and then checks the grid again (recursively).
-- If there are no more points to flash, returns the grid ready for next increment as well as the number of flashes performed.
resolve :: [[Int]] -> Set.Set Int -> ([[Int]], Int)
resolve grid flashed
    | length to_flash > 0 =
        let new_flashed = foldl (\acc (x, y) -> Set.insert (hash (x, y)) acc) flashed to_flash
            in resolve (perform_flashes grid to_flash) new_flashed
    | otherwise = (reset grid, length flashed)
        where to_flash = find_flashes grid flashed

-- Resets all points that may have reached numbers above 9 to 0.
reset :: [[Int]] -> [[Int]]
reset grid = [[if (grid !! row) !! col > 9 then 0 else (grid !! row) !! col | col <- [0..9]] | row <- [0..9]]

-- Hashing function used for sets.
hash :: (Int, Int) -> Int
hash (x, y) = 10 * x + y
