module T where
	import Debug.Trace


	data Node = Nil | Node {
		d :: Int,
		l :: Node,
		r :: Node
	} deriving (Show)

	insert :: Int -> Node -> Node
	insert item root = case root of
		Nil -> Node item Nil Nil
		val ->  case ((d val) == item) of
			True -> root
			False -> case ((d val) < item) of
				True -> Node (d val) (l val) (insert item (r val))
				False -> Node (d val) (insert item (l val)) (r val)



	removeFromParent :: Int -> Node -> Node -> Node
	removeFromParent item root parent = case root of 
		Nil ->  Nil
		val -> case ((d val) == item) of
			True -> removeNode val parent
			False -> Node (d root) (removeFromParent item (l val) val) (removeFromParent item (r val) val)

	removeNode :: Node -> Node -> Node
	removeNode node parent = 

		if (isEmpty (l node) && (isEmpty (r node))) then
			Nil
		else
			if(isEmpty (l node) && (not (isEmpty (r node)))) then		
				(r node)
			else
				if(not (isEmpty (l node)) && (isEmpty (r node))) then		
					(l node)
				else do
					let min = (findMin (r node) Nil)
					Node min (l node) (remove min (r node))
		--	--find a minimum value in the right subtree;
		--	--replace value of the node to be removed with found minimum. Now, right subtree contains a duplicate!
		--	--apply remove to the right subtree to remove a duplicate.

	isEmpty :: Node -> Bool
	isEmpty node = case node of
		Nil -> True
		val -> False

	remove :: Int -> Node -> Node
	remove item root = case root of
		Nil -> Nil
		val -> removeFromParent item root Nil



 	findMin :: Node -> Node -> Int 
 	findMin root parent = case (l root) of
 		Nil -> (d root)
 		val -> findMin (l root) root

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

	pp :: Node -> IO()
	pp root = prettyPrint root " "

	prettyPrint :: Node -> [Char] -> IO()
	prettyPrint root inset = case root of
		Nil -> putStr ""
		tree -> do
			putStr inset
			putStrLn (show (d tree))
			prettyPrint (l tree) (inset ++ " L ")
			prettyPrint (r tree)	(inset ++ " R ")