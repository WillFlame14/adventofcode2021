import qualified Data.Map as Map
import qualified Data.Set as Set

main = interact sol

-- This took way longer than it should have since I couldn't figure out how to modify a 1000x1000 2D array fast enough.
-- I probably could have expanded each line one at a time instead of all at once, but I don't actually think it makes that much of a difference.
sol :: String -> String
sol input =
    let contents = lines input
        lines2 = map parse contents
        filtered = filter (\(start, end) -> (start !! 0 == end !! 0) || (start !! 1 == end !! 1)) lines2
        expanded1 = concat (map expand filtered)
        expanded2 = concat (map expand lines2)
    in show (
        sum_instances expanded1 (Set.fromList ([] :: [Int])) (Map.fromList ([] :: [(Int, Int)])),
        sum_instances expanded2 (Set.fromList ([] :: [Int])) (Map.fromList ([] :: [(Int, Int)]))
    )

-- Given a list of expanded points, puts them into a frequency table and returns the number of points that have freq > 1
-- This is the only solution I could think of that is faster than O(N^2), so I needed to learn how to use Data.Set and Data.Map for fast hashing.
-- Using `elem` or linear search is too slow, and I wasn't sure if I could get some funky key pair thing working properly.
sum_instances :: [[Int]] -> Set.Set Int -> Map.Map Int Int -> Int
sum_instances [] contains hmap = Map.foldr (\val acc -> (if val > 1 then 1 else 0) + acc) 0 hmap
sum_instances (x:xs) contains hmap
    | not (Set.member hash contains)    = sum_instances xs (Set.insert hash contains) (Map.insert hash 1 hmap)
    | otherwise                         = sum_instances xs contains (Map.insertWith (+) hash 1 hmap)
        where hash = 1000 * (x !! 0) + (x !! 1)

-- Given a pair of start and end points, returns a list of all the points connecting both points (including start and end).
expand :: ([Int], [Int]) -> [[Int]]
expand ([x1, y1], [x2, y2])
    | x1 == x2 && y1 == y2  = [[x1, y1]]
    | otherwise             =
        let new_x = if x1 /= x2 then x1 + ((x2 - x1) `div` abs(x2 - x1)) else x1
            new_y = if y1 /= y2 then y1 + ((y2 - y1) `div` abs(y2 - y1)) else y1
            next = [new_x, new_y]
            in [x1, y1]:expand (next, [x2, y2])

-- Reads an input string into a start point and an end point
parse :: String -> ([Int], [Int])
parse x =
    let parts = words x
        start = read_pair (parts !! 0) [] []
        end = read_pair (parts !! 2) [] []
        in (start, end)

-- Reads "xxx,yyy" into [xxx, yyy]
read_pair :: String -> String -> [String] -> [Int]
read_pair [] new pair = map read (reverse ((reverse new):pair))
read_pair (x:xs) new pair
    | x == ','  = read_pair xs [] ((reverse new):pair)
    | otherwise = read_pair xs (x:new) pair
