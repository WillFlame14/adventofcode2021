import Data.List

main = interact sol

sol :: String -> String
sol input =
    let grid = lines input
        steps = iterate step grid
    in show (
        find_stall steps 0
    )

-- Given a list of steps, determines the first step when the sea cucumbers stop moving.
find_stall :: [[String]] -> Int -> Int
find_stall (s1:s2:steps) counter
    | counter == 10000  = -1
    | s1 == s2          = counter + 1
    | otherwise         = find_stall (s2:steps) (counter + 1)

-- Given a grid, performs a step (east movement followed by south movement).
step :: [String] -> [String]
step grid =
    let east_step = map (move_line '>') grid
        south_step = map (move_line 'v') (transpose east_step)
        in transpose south_step

-- Given a line and a "selection", tries to move the selection to the next spot if there is space (includes wrap-around).
move_line :: Char -> String -> String
move_line p xs =
    if (xs !! 0) == '.' && (xs !! (length xs - 1)) == p then
        (p:(tail . init $ move_line_nw p xs) ++ ['.'])
    else
        move_line_nw p xs

-- Given a line and a "selection", tries to move the selection to the next spot if there is space (no wrap-around).
move_line_nw :: Char -> String -> String
move_line_nw _ [] = []
move_line_nw _ [x] = [x]
move_line_nw p (x1:x2:xs) =
    if x1 == p && x2 == '.' then
        '.':p:(move_line_nw p xs)
    else
        x1:(move_line_nw p (x2:xs))
