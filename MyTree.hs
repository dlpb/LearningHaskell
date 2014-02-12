module T where

	data Node = Nil | Node {
		d :: Int,
		l :: Node,
		r :: Node
	} deriving (Show)

	insert :: Int -> Node -> Node
	insert item root = case root of
		Nil -> Node item Nil Nil
		val -> case ((d val) < item) of
			True -> Node (d val) (l val) (insert item (r val))
			False -> Node (d val) (insert item (l val)) (r val)

	remove :: Int -> Node -> Node
	remove item root = case root of
		Nil -> Nil
		val -> removeFromParent root Nil

    removeFromParent :: Int -> Node -> Node -> Node
    removeFromParent item root parent = case root of 
    	Nil -> Nil
    	val -> case ((d val) == item) of
    		True -> removeNode val parent
    		False -> Node item (removeFromParent item (l val) val) (removeFromParent item (r val) val)

    removeNode :: Node -> Node -> Node
    removeNode node parent = case node of
    	((l == Nil) && (r == Nil)) -> Nil
    	((l == Nil) && (r != Nil)) -> r
    	((l != Nil) && (r == Nil)) -> l
    	((l != Nil) && (r != Nil)) -> 


  	pt :: Node -> IO()
	pt root = do
		(printNode (l root))
		putStr (show (d root))
		putStr ","
		(printNode (r root))

	printNode :: Node -> IO()
	printNode root = case root of
		Nil -> putStr ""
		tree -> pt tree

	p :: Node -> IO()
	p root = do 
		(pt root)
		putStrLn ""