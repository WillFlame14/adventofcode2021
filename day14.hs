import qualified Data.Map as Map

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        start = contents !! 0
        rules = map parse $ drop 2 contents
        insert_map = foldl (\acc (rule, ins) -> Map.insert rule (ins !! 0) acc) Map.empty rules
        freq_map = init_freq start insert_map Map.empty
        freq_map1 = snd $ (iterate (\(x, y) -> (x, insertion2 x y)) (insert_map, freq_map)) !! 10
        freq_map2 = snd $ (iterate (\(x, y) -> (x, insertion2 x y)) (insert_map, freq_map)) !! 40
    in show (
        calc . map snd . Map.toList $ decompose freq_map1 (start !! (length start - 1)),
        calc . map snd . Map.toList $ decompose freq_map2 (start !! (length start - 1))
    )

calc :: [Int] -> Int
calc xs = maximum xs - minimum xs

-- Given a string, initializes the frequency table of pairs.
init_freq :: String -> Map.Map String Char -> Map.Map String Int -> Map.Map String Int
init_freq [x1] _ freq_map = freq_map
init_freq (x1:x2:xs) insert_map freq_map = Map.insertWith (+) [x1, x2] 1 $ init_freq (x2:xs) insert_map freq_map

-- Given a frequency table of pairs, performs 1 round of splitting on all pairs to generate a new frequency table.
insertion2 :: Map.Map String Char -> Map.Map String Int -> Map.Map String Int
insertion2 insert_map freq_map = foldl (\acc (string, freq) -> split string freq insert_map acc) freq_map $ Map.toList freq_map

-- Takes a pair and a frequency and updates the frequency table after trying to split the pair.
split :: [Char] -> Int -> Map.Map String Char -> Map.Map String Int -> Map.Map String Int
split [x1, x2] freq insert_map freq_map =
    case Map.lookup [x1, x2] insert_map of
        Nothing -> freq_map
        Just x -> Map.insertWith (+) [x1, x] freq . Map.insertWith (+) [x, x2] freq . Map.insertWith (+) [x1, x2] (-freq) $ freq_map

-- Takes a frequency table of pairs and an end char, returns a frequency table of the combined string.
decompose :: Map.Map String Int -> Char -> Map.Map Char Int
decompose freq_map end_char =
    let initial = Map.fromList [(end_char, 1)]
        in foldl (\acc (string, freq) -> Map.insertWith (+) (string !! 0) freq acc) initial $ Map.toList freq_map

-- Reads an input string into two parts.
parse :: String -> (String, String)
parse x = (parts !! 0, parts !! 2)
    where parts = words x

-- Naive insertion method that was used to build the string (not used in above solution)
insertion :: String -> Map.Map String Char -> String
insertion [x] _ = [x]
insertion (x1:x2:xs) insert_map =
    let ins = Map.lookup [x1, x2] insert_map
        rest = insertion (x2:xs) insert_map
        in case ins of
            Nothing -> x1:rest
            Just x -> x1:x:rest