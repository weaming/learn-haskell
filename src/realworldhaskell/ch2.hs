module Ch1 where

lastButOne :: [a] -> a
lastButOne (x : y : []) = x
lastButOne (x : y : ys) = lastButOne $ y : ys
