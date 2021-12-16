import Numeric (readHex)

main = interact sol

sol :: String -> String
sol input =
    -- For each character in input, we read it as hex, convert it to binary (in the form of int array), and pad it with leading 0s.
    -- Then we concatenate the int arrays.
    let binary = concat $ map (\x -> padBin . toBin . fst $ readHex [x] !! 0) input
    in show (
        fst $ read_packet 1 binary,
        fst $ read_packet 2 binary
    )

-- Given part 1 or part 2, takes a binary array and reads one packet.
-- Returns a pair of (calculated value, remaining binary array). 
read_packet :: Int -> [Int] -> (Int, [Int])
read_packet _ [] = (0, [])
read_packet part xs
    | length xs < 8 && sum xs == 0  = (0, [])   -- Trailing zeroes at the end of the input
    | otherwise =
        -- Split off parts that we need to parse while keeping track of the rest of the binary array.
        let (version_bin, rest) = splitAt 3 xs
            version = fromBin version_bin
            (type_bin, rest1) = splitAt 3 rest
            in case fromBin type_bin of
                4 ->
                    let (literal_bin, rest2) = read_literal rest1
                        in (if part == 1 then version else fromBin literal_bin, rest2)
                type_id ->
                    let (length_type, rest2) = splitAt 1 rest1
                        in case (length_type !! 0) of
                            0 ->
                                let (total_len_bin, rest3) = splitAt 15 rest2
                                    total_len = fromBin total_len_bin
                                    (sub_packet_bin, rest4) = splitAt total_len rest3
                                    -- Read subpackets from the sub_packet binary array until "rest" becomes empty.
                                    -- We need to take the final packet where "rest" becomes empty too, so takeWhileOne is used.
                                    sub_packets = takeWhileOne (\(_, rest5) -> not (null rest5)) . drop 1 $ iterate (\(_, rest5) -> read_packet part rest5) (0, sub_packet_bin)
                                    (result, _) = (if part == 1 then combine_sub_packets else calc_sub_packets type_id) sub_packets
                                    in (if part == 1 then version + result else result, rest4)
                            1 ->
                                let (num_sub_bin, rest3) = splitAt 11 rest2
                                    num_sub = fromBin num_sub_bin
                                    -- Keep reading until the specified number of sub_packets is reached.
                                    sub_packets = take num_sub . drop 1 $ iterate (\(_, rest4) -> read_packet part rest4) (0, rest3)
                                    (result, rest4) = (if part == 1 then combine_sub_packets else calc_sub_packets type_id) sub_packets
                                    in (if part == 1 then version + result else result, rest4)

-- Reimplementation of takeWhile, but includes the first element where the predicate is false as well.
takeWhileOne :: (a -> Bool) -> [a] -> [a]
takeWhileOne p = foldr (\x ys -> if p x then x:ys else [x]) []

-- Given a list of subpackets in the form (value, rest), adds the values together. (Used in part 1.)
combine_sub_packets :: [(Int, [Int])] -> (Int, [Int])
combine_sub_packets sub_packets =
    let result = foldl (\acc (x, _) -> acc + x) 0 sub_packets
        rest = snd $ (sub_packets !! (length sub_packets - 1))
        in (result, rest)

-- Given a list of subpackets in the form (value, rest) and an operation, performs the operation on the values. (Used in part 2.)
calc_sub_packets :: Int -> [(Int, [Int])] -> (Int, [Int])
calc_sub_packets op sub_packets
    | op == 0   = (sum vals, rest)
    | op == 1   = (product vals, rest)
    | op == 2   = (minimum vals, rest)
    | op == 3   = (maximum vals, rest)
    | op == 4   = (vals !! 0, rest)
    | op == 5   = (if vals !! 0 > vals !! 1 then 1 else 0, rest)
    | op == 6   = (if vals !! 0 < vals !! 1 then 1 else 0, rest)
    | op == 7   = (if vals !! 0 == vals !! 1 then 1 else 0, rest)
        where vals = map fst sub_packets
              rest = snd $ (sub_packets !! (length sub_packets - 1))

-- Reads a literal from a binary array, and returns a pair of (binary literal array, rest of array).
read_literal :: [Int] -> ([Int], [Int])
read_literal xs =
    let (cont:curr_bin, rest) = splitAt 5 xs
        in case cont of
            1 -> let (others, rest2) = read_literal rest
                    in (foldl (\acc x -> x:acc) others (reverse curr_bin), rest2)
            0 -> (curr_bin, rest)

padBin :: [Int] -> [Int]
padBin xs =
    let missing = (4 - (length xs `mod` 4)) `mod` 4
        in foldl (\acc x -> x:acc) xs $ replicate missing 0

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (bin_helper n)

bin_helper :: Int -> [Int]
bin_helper 0 = []
bin_helper n = let (q,r) = n `divMod` 2 in r : bin_helper q

fromBin :: [Int] -> Int
fromBin xs = foldl (\acc x -> x + 2 * acc) 0 xs