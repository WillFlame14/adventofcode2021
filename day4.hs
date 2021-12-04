import Data.Char

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        order = map read (words [if x == ',' then ' ' else x | x <- contents !! 0]) :: [Int]
        boards = split (tail (tail contents)) [] []
        (bingo_board1, last_called1) = play (zip boards [1..(length boards)]) order (0, 0)
        (bingo_board2, last_called2) = play2 (zip boards [1..(length boards)]) order (0, 0, 0)
    in show (
        (unmarked_sum bingo_board1 * last_called1),     -- part 1
        (unmarked_sum bingo_board2 * last_called2)      -- part 2
    )

-- Takes the input (excluding the called numbers) and two empty arrays as parameters.
-- Returns a list of 2D bingo boards.
split :: [String] -> [[Int]] -> [[[Int]]] -> [[[Int]]]
split [] y z = y:z
split (x:xs) y z
    | x == "" = split xs [] (y:z)
    | otherwise =
        let ints = [read a :: Int | a <- words x]
            in split xs (ints:y) z

-- Takes a bingo board and the number called, and replaces the number called (if it exists) with -10.
fill :: [[Int]] -> Int -> [[Int]]
fill xs called = [[if a == called then -10 else a | a <- x] | x <- xs]

-- Returns the number of bingos a board has.
bingo :: [[Int]] -> Int
bingo xs =
    let cols = [ sum [x !! y | x <- xs] | y <- [0..4] ]
        rows = [ sum x | x <- xs ]
        in length (filter (< -40) (cols ++ rows))

-- Returns the sum of unmarked numbers on a bingo board.
unmarked_sum :: [[Int]] -> Int
unmarked_sum xs = sum [sum [max a 0 | a <- x] | x <- xs]

-- Takes the zipped list of bingo boards and their indices, the list of called numbers, and a storage parameter.
-- Plays bingo until one board gets a bingo. Then returns the *marked* board and the last called number.
play :: [([[Int]], Int)] -> [Int] -> (Int, Int) -> ([[Int]], Int)
play all (called:order) (bingo_index, last_called)
    | bingo_index /= 0   = (fst (all !! (bingo_index - 1)), last_called)
    | otherwise          =
        let new_all = [(fill board called, index) | (board, index) <- all]
            found_bingo = (sum [if (bingo board) > 0 then i else 0 | (board, i) <- new_all], called)
        in play new_all order found_bingo

-- Takes the zipped list of bingo boards and their indices, the list of called numbers, and a storage parameter.
-- Plays bingo until all boards have at least one bingo. Then returns the board that got a bingo last and the last called number.
play2 :: [([[Int]], Int)] -> [Int] -> (Int, Int, Int) -> ([[Int]], Int)
play2 all [] (new_sum, last_called, last_sum) = (fst (all !! (new_sum - last_sum - 1)), last_called)
play2 all (called:order) (new_sum, last_called, last_sum)
    | new_sum == (((length all) + 1) * (length all) `div` 2) = (fst (all !! (new_sum - last_sum - 1)), last_called)
    | otherwise =
        let new_all = [(fill board called, index) | (board, index) <- all]
            results = (sum [if (bingo board) > 0 then i else 0 | (board, i) <- new_all], called, new_sum)
        in play2 new_all order results

