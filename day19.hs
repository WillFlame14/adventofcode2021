import Data.List
import Data.Maybe
import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified Data.Map as Map

main = interact sol

-- This solution considers scanner 0 to be located at [0, 0, 0] and gets all other scanners to orient themselves the same way.

sol :: String -> String
sol input =
    let contents = lines input
        readings = parse contents
        beacons = foldl (\acc x -> Set.insert (show x) acc) Set.empty (readings !! 0)
        (all_beacons, offset_map) = match readings beacons $ Map.fromList [(0, [0, 0, 0])]
        scanner_locs = Map.toList offset_map
        sl_len = length scanner_locs - 1
    in show (
        Set.size all_beacons,
        -- Compare every unique pair of scanner locations and find the one with the maximum distance.
        maximum [maximum [let (_, offset1) = (scanner_locs !! s1); (_, offset2) = (scanner_locs !! s2) in dist offset1 offset2 | s1 <- [(s2 + 1)..sl_len]] | s2 <- [0..(sl_len - 1)]]
    )

-- Breaks the input into lists of scanner readings.
parse :: [String] -> [[[Int]]]
parse xs =
    let (chunk_raw, rest) = break (== "") xs
        chunk = map (\x -> map read (wordsWhen (==',') x) :: [Int]) $ tail chunk_raw
        in chunk:(if null rest then [] else parse $ tail rest)

-- Takes the list of all scanner readings, a set of beacons found so far, and a map of scanner_index -> offset from [0, 0, 0].
-- Returns the set and map after recursively trying to match as many scanners as it can.
match :: [[[Int]]] -> Set.Set String -> Map.Map Int [Int] -> (Set.Set String, Map.Map Int [Int])
match scanners set offset_map =
    let len = length scanners - 1
        -- Compare all pairs of scanner readings, where one has alreaady been connected and the other has not.
        matches = [[(compare2 (scanners !! s1) (scanners !! s2) 0, (s1, s2)) | s1 <- [0..len], not (isNothing (Map.lookup s1 offset_map))] | s2 <- [0..len], isNothing (Map.lookup s2 offset_map)]
        -- Search the results array above for a match. (This search allows the comparison above to be lazy.)
        matched = dropWhile isNothing [find (\((orientation, _, _), _) -> orientation !! 0 /= 0) match | match <- matches]
    in (if null matched then
            -- If there are no matches found, return the set of beacons that we have located as well as the map of scanner positions.
            (set, offset_map)
        else
            -- We found a match, so fix the unknown beacon's orientation and figure out its distance from [0, 0, 0]. Then recursively call this function.
            -- This function is *extremely* slow (O(N^4)), so I have a debug trace indicating the number of connected beacons so far.
            let ((orientation, offset, new_pts), (s1, s2)) = trace ("connected " ++ show (Map.size offset_map + 1)) (fromJust $ head matched)
                true_offset = zipWith (+) offset (fromJust $ Map.lookup s1 offset_map)
                adjusted_pts = map (\point -> zipWith (+) point true_offset) new_pts
                -- Put the adjusted points into the beacon set.
                new_set = foldl (\acc x -> Set.insert (show x) acc) set adjusted_pts
                -- Put the correctly-oriented (but not adjusted) points into scanners.
                new_scanners = replace s2 new_pts scanners
                new_offset_map = Map.insert s2 true_offset offset_map
                in match new_scanners new_set new_offset_map
        )

-- Finds the Manhattan distance between two points.
dist :: [Int] -> [Int] -> Int
dist loc1 loc2 = sum . map abs $ zipWith (-) loc1 loc2

-- Right-hand rule coming in to save me while generating all these orientations. 1 is x, 2 is y, 3 is z.
orientations = [
    [1, 2, 3], [1, -2, -3], [1, 3, -2], [1, -3, 2],
    [-1, 2, -3], [-1, -2, 3], [-1, 3, 2], [-1, -3, -2],
    [2, 1, -3], [2, -1, 3], [2, 3, 1], [2, -3, -1],
    [-2, 1, 3], [-2, -1, -3], [-2, 3, -1], [-2, -3, 1],
    [3, 1, 2], [3, -1, -2], [3, 2, -1], [3, -2, 1],
    [-3, 1, -2], [-3, -1, 2], [-3, 2, 1], [-3, -2, -1]]

-- Given two lists of scanner readings, tries all possible orientations and permutations to get the 2nd list to match at least 12 beacons in the 1st list.
-- Returns a tuple of (correct orientation, relative offset from 2 to 1, correctly-oriented 2nd list of scanner readings).
-- This could be optimized by storing the hashsets of connected scanners instead of generating them each time, but I don't want to break this program again.
compare2 :: [[Int]] -> [[Int]] -> Int -> ([Int], [Int], [[Int]])
compare2 p1s p2s orient_index
    -- If orient_index hits 24, we are unable to find an orientation or permutation to get the 2nd list to match the 1st list.
    | orient_index == 24    = ([0, 0, 0], [0, 0, 0], [[0]])
    | otherwise =
        let orientation = orientations !! orient_index
            -- Re-orient the 2nd list of points according to the orientation.
            new_p2s = gen_orientation p2s orientation
            -- Find all possible offsets by using every pair of points.
            offsets = concat [[zipWith (-) p1 p2 | p1 <- p1s] | p2 <- new_p2s]
            -- Hash all of the points in list 1 for fast "contains()" checking.
            hash_p1s = foldl (\acc x -> Set.insert (show x) acc) Set.empty p1s
            -- Find the number of matches between both lists.
            matches = map (test (hash_p1s) new_p2s) offsets
            in case findIndex (>= 12) matches of
                Just x -> (orientation, offsets !! x, new_p2s)
                -- If there is no offset that works, try again with a different orientation.
                Nothing -> compare2 p1s p2s (orient_index + 1)

-- Given a set of points and an orientation, transforms the points according to the orientation.
gen_orientation :: [[Int]] -> [Int] -> [[Int]]
gen_orientation points [ox, oy, oz] = map (\point -> map (select point) [ox, oy, oz]) points

-- Helper function for gen_orientation.
select :: [Int] -> Int -> Int
select point o = (abs o `div` o) * point !! ((abs o) - 1)

-- Given a set of points and a list of other points, finds the number of matching points.
test :: Set.Set String -> [[Int]] -> [Int] -> Int
test _ [] _ = 0
test set (p2:rest2) offset =
    (if Set.member (show (zipWith (+) p2 offset)) set then 1 else 0) + test set rest2 offset

-- Splits a string on a predicate.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- Takes an index, a new element, and an array. Replaces the element at that index of the array.
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 y (x:xs) = y:xs
replace n y (x:xs) = x : replace (n - 1) y (xs)
