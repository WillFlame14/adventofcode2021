import Data.Char

data Snailfish = Leaf Int | Pair Snailfish Snailfish deriving (Show)

main = interact sol

sol :: String -> String
sol input =
    let contents = map parse $ lines input
    in show (
        magnitude $ foldl1 add contents,
        maximum [magnitude (add x y) | x <- contents, y <- contents]
    )

-- Adds two snailfish numbers.
add :: Snailfish -> Snailfish -> Snailfish
add x y = reduce $ Pair x y

-- Reduces a snailfish number completely.
reduce :: Snailfish -> Snailfish
reduce (Leaf x) = Leaf x
reduce x =
    let (after_explode, (left_prop, right_prop), exploded) = explode x 0
        in (if exploded == 1 then
                reduce after_explode
            else
                let (after_split, had_split) = split after_explode
                    in (if had_split == 1 then reduce after_split else after_split)
            )

-- Explodes a snailfish number, if possible. Returns the exploded snailfish number, some propagation info (internal use only), and whether an explosion occurred.
-- The propagation info is in the form of (left_prop, right_prop), and is used when a snailfish number explodes in order to propagate the explosion correctly.
explode :: Snailfish -> Int -> (Snailfish, (Int, Int), Int)
explode (Leaf x) _ = (Leaf x, (-1, -1), 0)
explode (Pair left right) depth
    -- left and right may need to explode.
    | depth + 1 == 4    = case left of
        (Leaf x) -> case right of
            -- Left is leaf, right is leaf. No exploding required.
            (Leaf y) -> (Pair left right, (-1, -1), 0)
            -- Left is leaf, right is pair. Right explodes onto left, and propagates rr outwards to the right.
            (Pair (Leaf rl) (Leaf rr)) -> (Pair (Leaf (x + rl)) (Leaf 0), (-1, rr), 1)
            catch -> error "Tried to explode right pair not composed of two leaves"
        (Pair (Leaf ll) (Leaf lr)) ->
            case right of
                -- Left is pair, right is leaf. Left explodes onto right, and propagates ll outwards to the left.
                (Leaf y) -> (Pair (Leaf 0) (Leaf (lr + y)), (ll, -1), 1)
                -- Left is pair, right is pair. Left explodes onto right, so the leftmost number in right gets lr added to it.
                -- ll gets propagated to the left.
                (Pair _ _) -> (Pair (Leaf 0) (add_left right lr), (ll, -1), 1)
        catch -> error "Tried to explode left pair not composed of two leaves"
    | otherwise =
        -- Not deep enough to explode immediately, try to explode the left side after going one layer down.
        let (new_left, (left_prop, right_prop), left_exploded) = explode left (depth + 1)
            in (if left_exploded == 0 then
                    -- Left didn't explode, try exploding right.
                    let (new_right, (left_prop2, right_prop2), right_exploded) = explode right (depth + 1)
                        in (if right_exploded == 0 then
                                -- Nothing to explode.
                                (Pair left right, (-1, -1), 0)
                            else
                                -- Right exploded, so we resolve its left propagation.
                                (Pair (if left_prop2 /= -1 then (add_right left left_prop2) else left) new_right, (-1, right_prop2), 1)
                            )
                else
                    -- Left exploded, so we resolve its right propagation here.
                    (Pair new_left (if right_prop /= -1 then (add_left right right_prop) else right), (left_prop, -1), 1)
                )

-- Splits a snailfish number, if possible. The 2nd element of the pair is 1 if a split was made, and 0 otherwise.
split :: Snailfish -> (Snailfish, Int)
split (Leaf x)
    | x >= 10 =
        let left = x `div` 2
            right = x - left
            in (Pair (Leaf left) (Leaf right), 1)
    | otherwise = (Leaf x, 0)
split (Pair left right) =
    let (new_left, left_split) = split left
        in case left_split of
            1 -> (Pair new_left right, 1)
            0 ->
                let (new_right, right_split) = split right
                    in (Pair left new_right, right_split)

-- Finds the magnitude of a snailfish number.
magnitude :: Snailfish -> Int
magnitude (Leaf x) = x
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right

-- Given a snailfish number and a regular number, adds the regular number to the leftmost regular number in the snailfish number.
add_left :: Snailfish -> Int -> Snailfish
add_left (Leaf x) y = Leaf (x + y)
add_left (Pair l r) y = Pair (add_left l y) r

-- Same as add_left, but right.
add_right :: Snailfish -> Int -> Snailfish
add_right (Leaf x) y = Leaf (x + y)
add_right (Pair l r) y = Pair l (add_right r y)

-- Helper function for parse so I don't need to provide all the weird extra parameters.
parse :: String -> Snailfish
parse x = parse1 x [] 0

-- Parses a string into a snailfish number.
-- Tracks whether a number is currently being pushed into the stack (num_ongoing), otherwise goes by character.
-- Once a closing bracket arrives, we create a pair from the two leaves on top of the stack.
parse1 :: String -> [Snailfish] -> Int -> Snailfish
parse1 [] stack _ = head stack
parse1 (x:xs) stack num_ongoing
    | x `elem` ['0'..'9'] =
        if num_ongoing == 1 then
            let ((Leaf x1):rest) = stack
                x2 = digitToInt x
                in parse1 xs ((Leaf $ 10 * x1 + x2):rest) 1
        else
            parse1 xs ((Leaf $ digitToInt x):stack) 1
    | x == ']'              = let (x1:x2:rest) = stack in parse1 xs ((Pair x2 x1):rest) 0
    | otherwise             = parse1 xs stack 0
