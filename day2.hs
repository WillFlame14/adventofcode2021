main = interact sol2

-- Part 1 is relatively simple, but it took me a while to think of how to write it.
-- I filter for the specific command(s) and then sum the resulting arrays.
sol1 :: String -> String
sol1 input =
    let contents = lines input
        steps = [words x | x <- contents]
        forwards = sum [read distance :: Int | [command, distance] <- steps, command == "forward"]
        depth = sum [(read distance :: Int) * (if command == "up" then -1 else 1) | [command, distance] <- steps, command `elem` ["up", "down"]]
    in show (forwards * depth)

-- Part 2 was more complex, since I couldn't come up with a good way to store the current aim in a recursive solution.
-- First, an array of the current aims is generated. Then it's zipped together with the list of steps.
-- This allows pure computation of depth (i.e. separately from horizontal distance).
-- The horizontal distance is calculated the same way as part 1.
sol2 :: String -> String
sol2 input =
    let contents = lines input
        steps = [words x | x <- contents]
        aims = tail (reverse (aim (reverse steps)))
        combined = zip steps aims
        forwards = sum [read distance :: Int | [command, distance] <- steps, command == "forward"]
        depth = sum [(read distance :: Int) * (if command == "up" then -1 else 1) | [command, distance] <- steps, command `elem` ["up", "down"]]
    in show (forwards * (depth2 combined))

-- Creates an array of aims given a set of steps.
-- However, this is recursively generated from the right, so the steps need to be reversed and then the final array needs to be reversed.
-- I really need to learn how to recursively generate from the left.
-- Also, there's an extra 0 at the end of the list that needs to be removed.
aim :: [[String]] -> [Int]
aim [] = [0]
aim ([command,distance]:xs)
    | command == "up"       = (-change + last):aim xs
    | command == "down"     = (change + last):aim xs
    | command == "forward"  = last:aim xs
    where change = read distance :: Int
          last = head (aim xs)

-- Finds the final depth of the submarine given a set of steps zipped together with the current aim.
depth2 :: [([String], Int)] -> Int
depth2 [] = 0
depth2 (([command,distance], aim):xs)
    | command `elem` ["up", "down"] = depth2 xs
    | command == "forward"          = (read distance :: Int) * aim + (depth2 xs)

