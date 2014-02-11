module D where

data Cell value = Nil | Cell 
	{ 
		h :: value, 
		t :: Cell value 
	} deriving (Show)

cons :: value -> Cell value -> Cell value
cons x xs = 
	Cell x xs

head :: Cell value -> value
head x = h x

tail :: Cell value -> Cell value
tail x = t x

empty :: Cell value -> Bool
empty xs = case xs of
	Nil -> True
	_ -> False

map :: (a -> b) -> Cell a -> Cell b
map f xs = case xs of
	Nil -> Nil
	x -> 
		cons 
			(f (D.head x))
			(D.map f (D.tail x))
