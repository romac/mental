
module Example where

id = \x -> (x, 1)

id2 = \x -> x

id3 = id2

type Id = a -> a

id3 : Id
id3 = \x -> x

one : Int
one = let x = 1 in x


