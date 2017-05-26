
module Example where

id : a -> (a, Int)
id = \x -> (x, 1)

id2 : a -> a
id2 = \x -> x

id3 : a -> a
id3 = id2

twice : Bool -> Bool -> Bool
twice = \x -> \y -> if x then x else y

type Id = a -> a

id4 : Id
id4 = \x -> x

one : Int
one = let x = 1 in id3 x

two : Bool
two = let x = True in id2 x

