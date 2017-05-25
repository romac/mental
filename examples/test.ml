
module Example where

id = \x -> (x, 1)

id2 = \x -> x

id3 = id2

twice = \x -> \y -> if x then x else y

-- type Id = a -> a

-- id4 : Id
-- id4 = \x -> x

-- one : Int
one = let x = 1 in id3 x

-- two = let x = True in id2 x
