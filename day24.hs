-- I did today by hand. Below are my scratch notes with some associated thoughts.

-- Each chunk does this:
-- inp w
-- reset x
-- x = (z % 26) + A
-- if A < 0
--     z /= 26
-- if x != w
--     z *= 26
--     z += (w + B)

-- Treating z as a base 26 string, the program becomes:
-- if A is negative
--      pop last letter of z
-- if (popped/last letter of z) + A is not input
--     append (input + B) to z

-- Input in pairs (A, B):
-- (12, 4)
-- (11, 11)
-- (13, 5)
-- (11, 11)
-- (14, 14)
-- (-10, 7) <-- z /= 26 between x and z modifications
-- (11, 11)
-- (-9, 4) <-- z /= 26 between x and z modifications
-- (-3, 6) <-- z /= 26 between x and z modifications
-- (13, 5)
-- (-5, 9) <-- z /= 26 between x and z modifications
-- (-10, 12) with div
-- (-4, 14) with div
-- (-5, 14) with div

-- w10 + 5 - 5 = w11
-- w7 + 11 - 9 = w8
-- w5 + 14 - 10 = w6
-- w4 + 11 - 3 = w9
-- w3 + 5 - 10 = w12
-- w2 + 11 - 4 = w13
-- w1 + 4 - 5 == w14

-- Part 1:
-- w1 = 9
-- w2 = 2
-- w3 = 9
-- w4 = 1
-- w5 = 5
-- w6 = 9
-- w7 = 7
-- w8 = 9
-- w9 = 9
-- w10 = 9
-- w11 = 9
-- w12 = 4
-- w13 = 9
-- w14 = 8
-- 92915979999498

-- Part 2:
-- w1 = 2
-- w2 = 1
-- w3 = 6
-- w4 = 1
-- w5 = 1
-- w6 = 5
-- w7 = 1
-- w8 = 3
-- w9 = 9
-- w10 = 1
-- w11 = 1
-- w12 = 1
-- w13 = 8
-- w14 = 1
-- 21611513911181


-- Some earlier thoughts while trying an approach starting from the end:

-- (-10, 12) with div
-- assuming cancel

-- w == (z % 26) - 10
-- z = (11..19) * 26 + (11..19) w = (1..9)

-- ------------------------

-- (-4, 14) with div
-- since B = +14, must cancel to remain between 6 and 14 for next step

-- w == (z % 26) - 4
-- z = (6..14) * 26 + (5..13), w = (1..9)

-- ------------------------

-- (-5, 14) with div
-- must cancel

-- w == (z % 26) - 5
-- z = (6..14), w = (1..9)

-- then z = 0
