module ListComprehensions where

mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]

res0 = [x | x <- mySqr, rem x 2 == 0]

res1 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

res2 = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

-- First write an expression that will make tuples of the outputs of mySqr and myCube.
-- Now alter that expression so that it only uses the x and y values that are less than 50.
-- how many tuples inhabit your output list.

res3 = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
