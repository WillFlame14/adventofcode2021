import qualified Data.Set as Set

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        adjacent1 = map (wordsWhen (== '-')) contents
        adjacent = concat [[x, reverse x] | x <- adjacent1]
    in show (
        length $ travel "start" adjacent (Set.fromList ["start"]) ["start"],
        length $ travel2 "start" adjacent (Set.fromList ["start"]) "" ["start"]
    )

-- Splits a string on a predicate.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- Finds all paths to end using BFS, only visiting small caves once.
travel :: String -> [[String]] -> Set.Set String -> [String] -> [[String]]
travel curr adjacent visited path
    | curr == "end" = [reverse path]
    | otherwise     =
        let potentials = map (!! 1) $ filter (\[x, y] -> curr == x && not (Set.member y visited)) adjacent
            new_visited new
                | is_small new  = Set.insert new visited
                | otherwise     = visited
            in concat [travel new adjacent (new_visited new) (new:path) | new <- potentials]

-- Finds all paths to end using BFS, but allowing one cave to be visited twice (called "extra").
travel2 :: String -> [[String]] -> Set.Set String -> String -> [String] -> [[String]]
travel2 curr adjacent visited extra path
    | curr == "end" = [reverse path]
    | extra /= ""   =
        -- If we already found an extra, this is the same algorithm as part 1.
        let potentials = map (!! 1) $ filter (\[x, y] -> curr == x && not (Set.member y visited)) adjacent
            new_visited new
                | is_small new  = Set.insert new visited
                | otherwise     = visited
            in concat [travel2 new adjacent (new_visited new) extra (new:path) | new <- potentials]
    | otherwise     =
        -- If we haven't found an extra yet, we don't filter out visited caves when looking for potentials (except "start").
        -- Instead, if our "new" cave has already been visited, we assign it to "extra".
        let potentials = map (!! 1) $ filter (\[x, y] -> curr == x && y /= "start") adjacent
            new_visited new
                | is_small new  = Set.insert new visited
                | otherwise     = visited
            new_extra new
                | is_small new && Set.member new visited = new
                | otherwise     = extra
            in concat [travel2 new adjacent (new_visited new) (new_extra new) (new:path) | new <- potentials]

-- Determines if a cave is small or not.
is_small :: String -> Bool
is_small cave = (cave !! 0) `elem` ['a'..'z']
        