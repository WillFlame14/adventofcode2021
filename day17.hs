main = putStrLn $ show (
    solve 1 ((207, 263), (-115, -63)),
    solve 2 ((207, 263), (-115, -63)))

solve :: Int -> ((Int, Int), (Int, Int))  -> Int
solve part range@((min_x, max_x), (min_y, max_y)) =
    -- Find the max y-value for every potential set of initial velocities!
    let maxs = map (\vel_x -> map (\vel_y -> possible (0, 0) (vel_x, vel_y) range 0) [min_y..(-min_y)]) [1..max_x]
        in case part of
            1 -> maximum $ map maximum maxs
            2 -> sum $ map (\row -> length $ filter (/= -1) row) maxs

-- Given a point, velocity vector and target range, determines if the target is reachable (recursively).
-- Returns the maximum y-value if the target is reachable, and -1 otherwise.
possible :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int)) -> Int -> Int
possible (x, y) (vel_x, vel_y) range@((min_x, max_x), (min_y, max_y)) y_max
    | x >= min_x && x <= max_x && y >= min_y && y <= max_y  = maximum [y, y_max]
    | x > max_x || y < min_y      = -1
    | otherwise =
        let (new_point, new_vel) = travel (x, y) (vel_x, vel_y)
            in possible new_point new_vel range $ maximum [y, y_max]

-- Given a point and a velocity vector, returns a pair of (new_point, new_velocity).
travel :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
travel (x, y) (vel_x, vel_y) =
    let new_vel_x = vel_x + (if vel_x == 0 then 0 else (-vel_x) `div` abs(vel_x))
        new_vel_y = vel_y - 1           
        in ((x + vel_x, y + vel_y), (new_vel_x, new_vel_y))
