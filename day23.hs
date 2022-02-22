import Data.List
import Data.Maybe
import qualified Data.Heap as Heap
import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified Data.Map as Map

main = interact sol

-- There are 11 hallway positions. The rooms are at index 2, 4, 6, and 8.
-- For part 1, change the max_height, positions map, indexes map and modify the termination condition for find_connected to 8 instead of 16.

-- I can't believe I spent so long writing my own heap implementation only for it to be too slow.
-- Haskell lists pls

max_height = 4

sol :: String -> String
sol input =
    let contents = lines input
        positions = Map.fromList [
            ("A1", (4, 1)), ("A2", (4, 4)), ("A3", (6, 3)), ("A4", (8, 2)),
            ("B1", (6, 4)), ("B2", (8, 4)), ("B3", (6, 2)), ("B4", (4, 3)),
            ("C1", (6, 1)), ("C2", (8, 1)), ("C3", (4, 2)), ("C4", (8, 3)),
            ("D1", (2, 1)), ("D2", (2, 4)), ("D3", (2, 2)), ("D4", (2, 3))]
        -- positions = Map.fromList [("B1", (2, 1)), ("A1", (2, 2)), ("C1", (4, 1)), ("D1", (4, 2)), ("B2", (6, 1)), ("C2", (6, 2)), ("D2", (8, 1)), ("A2", (8, 2))]
        hallway = Set.fromList []
        node = Node 0 positions hallway
    in show (
        dijkstra (Heap.fromList [node]) Set.empty
    )

dijkstra :: Heap.Heap Node -> Set.Set Int -> Int
dijkstra queue visited =
    if null queue then
        error "Ran out of nodes!"
    else 
        let curr = Heap.minimum queue
            rest = Heap.deleteMin queue
            pos = positions curr
            connected = find_connected pos 0 (hallway curr) (weight curr) visited
            new_visited = Set.insert (hash pos) visited
            new_queue = foldl (\acc node -> Heap.insert node acc) rest connected
            in 
                if Set.member (hash pos) visited then
                    dijkstra rest visited
                else if all (\x -> correct_room x pos == max_height) ['A'..'D'] then
                    weight curr
                else
                    dijkstra new_queue new_visited

indexes = Map.fromList [
    (0, "A1"), (1, "A2"), (2, "B1"), (3, "B2"), (4, "C1"), (5, "C2"), (6, "D1"), (7, "D2"),
    (8, "A3"), (9, "A4"), (10, "B3"), (11, "B4"), (12, "C3"), (13, "C4"), (14, "D3"), (15, "D4")]

find_connected :: Map.Map String (Int, Int) -> Int -> Set.Set Int -> Int -> Set.Set Int -> [Node]
find_connected positions 16 _ _ _ = []
find_connected positions index hallway weight visited =
    let str = fromJust $ Map.lookup index indexes
        (x, y) = fromJust $ Map.lookup str positions
        char = head str
        correct_col = fromJust $ Map.lookup char correct
        try_next = find_connected positions (index + 1) hallway weight visited
        in (if y == 0 then
                -- In hallway
                let correct_fill = correct_room char positions
                    in (if correct_fill < 0 then
                        -- Destination room is not filled correctly! We can't move.
                        -- trace (str ++ " dest not filled correctly") $ 
                        try_next
                    else
                        if any (\x1 -> Set.member x1 hallway) [((min correct_col x) + 1)..((max correct_col x) - 1)] then
                            -- Hallway path is obstructed! We can't move.
                            -- trace (str ++ " hallway obstructed") $
                            try_next
                        else
                            -- Move into the room, as far as we can.
                            let new_y = max_height - correct_fill
                                -- trace ("moving1 " ++ str ++ " to " ++ (show (correct_col, new_y))) $ 
                                new_positions = Map.insert str (correct_col, new_y) positions
                                new_hallway = Set.delete x hallway
                                new_weight = weight + (abs (correct_col - x) + new_y) * (fromJust $ Map.lookup char weights)
                                new_node = Node new_weight new_positions new_hallway
                                in
                                    if not (Set.member (hash new_positions) visited) then
                                        -- trace ("added node1 " ++ (show new_node)) $ 
                                        new_node:try_next
                                    else try_next
                    )
            -- In a room
            else
                -- This amphipod is in the correct location.
                if correct_room char positions > 0 && x == correct_col then
                    -- trace (str ++ " correct") $ 
                    try_next
                else if y > 1 && any (\(_, (x1, y1)) -> x1 == x && y1 < y) (Map.toList positions) then
                    -- Blocked by amphipod in front
                    -- trace (str ++ " blocked in room") $ 
                    try_next
                else
                    -- Move to any valid hallway position.
                    let possible_positions = gen_positions x hallway
                        in foldl (\acc x1 ->
                            -- trace ("moving2 " ++ str ++ " to " ++ (show (x1, 0))) $ 
                            let new_positions = Map.insert str (x1, 0) positions
                                new_hallway = Set.insert x1 hallway
                                new_weight = weight + (abs (x1 - x) + y) * (fromJust $ Map.lookup char weights)
                                new_node = Node new_weight new_positions new_hallway
                                in
                                    if not (Set.member (hash new_positions) visited) then
                                        -- trace ("added node2 " ++ (show new_node)) $ 
                                        new_node:acc
                                    else acc
                            ) try_next possible_positions
            )

correct = Map.fromList [('A', 2), ('B', 4), ('C', 6), ('D', 8)]
weights = Map.fromList [('A', 1), ('B', 10), ('C', 100), ('D', 1000)]

data Node = Node {
    weight :: Int,
    positions :: Map.Map String (Int, Int),
    hallway :: Set.Set Int
    } deriving (Show, Eq, Ord)

-- build_heap :: (Ord a) => [a] -> [a]
-- build_heap array =
--     let array_inds = zip array [0..(length array - 1)]
--         start = length array `div` 2 + 1
--         in map fst $ foldl (\acc x -> fix_down acc x) array_inds [start, (start - 1)..0]

-- fix_down :: (Ord a) => [(a, Int)] -> Int -> [(a, Int)]
-- fix_down array index
--     | index < 0 || index >= length array = array
--     | otherwise =
--         let left_ind = 2 * (index + 1) - 1
--             right_ind = 2 * (index + 1)
--             left = at left_ind array
--             right = at right_ind array
--             curr = at index array
--             pool = map fromJust $ filter (\x -> not $ isNothing x) [left, right, curr]
--             (min_key, min_ind) = foldl1 (\acc new -> if fst new < fst acc then new else acc) pool
--             in
--                 if min_ind == index then
--                     array
--                 else
--                     replace (\(_, i) -> i == index) (min_key, index) $ fix_down (replace (\(_, i) -> i == min_ind) (fst $ fromJust curr, min_ind) array) min_ind

-- pop_heap :: (Ord a) => [a] -> (a, [a])
-- pop_heap array = (array !! 0, build_heap $ tail array)

-- insert_heap :: (Ord a) => a -> [a] -> [a]
-- insert_heap node array = map fst $ fix_up (zip (array ++ [node]) [0..(length array)]) (length array)

-- fix_up :: (Ord a) => [(a, Int)] -> Int -> [(a, Int)]
-- fix_up array 0 = array
-- fix_up array index =
--     let parent_ind = index `div` 2
--         parent = fst $ array !! parent_ind
--         curr = fst $ array !! index
--         in
--             if parent > curr then
--                let new_array = replace (\(_, i) -> i == parent_ind) (curr, parent_ind) $ replace (\(_, i) -> i == index) (parent, index) array
--                 in fix_up new_array parent_ind
--             else
--                 array

gen_positions :: Int -> Set.Set Int -> [Int]
gen_positions x hallway =
    let right = takeWhile (\x1 -> not $ Set.member x1 hallway) [(x+1)..10]
        left = reverse $ takeWhile (\x1 -> not $ Set.member x1 hallway) [x,(x-1)..0]
        in filter (\x1 -> not $ x1 `elem` [2,4,6,8]) $ left ++ right

-- Determines how correctly filled a character's room is.
correct_room :: Char -> Map.Map String (Int, Int) -> Int
correct_room char positions =
    let correct_col = fromJust $ Map.lookup char correct
        in foldl (\acc i ->
            let str = fromJust $ Map.lookup i indexes
                (x, y) = fromJust $ Map.lookup str positions
                in
                    if x == correct_col then
                        if head str /= char then -99 else 1 + acc
                    else acc
            ) 0 [0..(4*max_height - 1)]
    -- in (if x == fromJust (Map.lookup char correct) && y >= 1 then
    --         if (s !! 0) /= char then -99 else 1 + rest
    --     else rest)

hash :: Map.Map String (Int, Int) -> Int
hash positions = foldl (\acc i ->
    let str = fromJust $ Map.lookup i indexes
        (x, y) = fromJust $ Map.lookup str positions
        in 121 * acc + 11 * x + y
    ) 0 [0..(4*max_height - 1)]

-- Takes a predicate, a new element, and an array. Replaces the element at the first index where the predicate is true.
replace :: (a -> Bool) -> a -> [a] -> [a]
replace _ _ [] = []
replace p y (x:xs)
    | p x = y:xs
    | otherwise = x:(replace p y xs)

at :: Int -> [a] -> Maybe a
at i xs
    | i < 0 || i >= length xs   = Nothing
    | otherwise                 = Just (xs !! i)
