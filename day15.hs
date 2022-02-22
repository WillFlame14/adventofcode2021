import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Heap as Heap

main = interact sol

-- I initially tried greedy dp from the bottom-right, but this unfortunately didn't work because my input path travels upwards and leftwards a little.
-- Part 1 should just be removing the *5 on max_dim in a few places.

max_dim = 100

sol :: String -> String
sol input =
    let contents = lines input
        grid = [map digitToInt line | line <- contents]
    in show (
        dijkstra (Heap.fromList [Node 0 (0, 0)]) grid Set.empty
    )

connected = [(-1, 0), (1, 0), (0, -1), (0, 1)]

dijkstra :: Heap.Heap Node -> [[Int]] -> Set.Set Int -> Int
dijkstra queue grid visited =
    if null queue then
        error "Ran out of nodes!"
    else
        let curr = Heap.minimum queue
            rest = Heap.deleteMin queue
            loc = location curr
            curr_hash = hash loc
            in
                if Set.member curr_hash visited then
                    dijkstra rest grid visited
                else if loc == (max_dim*5 - 1, max_dim*5 - 1) then
                    weight curr
                else
                    let connected = find_connected curr grid
                        new_queue = foldl (\acc node -> Heap.insert node acc) rest connected
                        new_visited = Set.insert curr_hash visited
                        in dijkstra new_queue grid new_visited

data Node = Node {
    weight :: Int,
    location :: (Int, Int)
} deriving (Show, Eq, Ord)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

find_connected :: Node -> [[Int]] -> [Node]
find_connected curr grid =
    let adjacent = filter valid $ map (add (location curr)) connected
        in map (\(x, y) ->
            let (rx, ry) = (x `mod` max_dim, y `mod` max_dim)
                extra_weight = ((grid !! ry) !! rx) + (x `div` max_dim) + (y `div` max_dim)
                new_weight = (weight curr) + ((extra_weight - 1) `mod` 9 + 1)
            in Node new_weight (x, y)
        ) adjacent

valid :: (Int, Int) -> Bool
valid (x, y) = (x >= 0 && x < 5*max_dim) && (y >= 0 && y < 5*max_dim)

hash :: (Int, Int) -> Int
hash (x, y) = 5*max_dim*x + y