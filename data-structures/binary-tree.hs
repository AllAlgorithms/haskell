-- Author: <github.com/monikanana>
-- implementation of binary tree that can take any data type 

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Ord, Eq) 

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a left right)
	| x == a = Node a left right
	| x < a = Node a (insert x left) right
	| otherwise = Node a left (insert x right)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty tree = False

search :: (Ord a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node a left right)
	| x == a = True
	| x < a  = search x left
	| otherwise = search x right

toString :: (Show a) => Tree a -> String
toString Empty = ""
toString (Node a left right)
	= show a ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")" 
