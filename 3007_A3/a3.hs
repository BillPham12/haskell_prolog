type Node = Int
type Edge = (Node, Node)
data Graph = Single Node |Union Graph Graph [Edge] deriving Show

-- Question 1 : " Does this Node Appear in the Graph"
containNode :: Node -> Graph -> Bool
containNode node (Union x y z)  = containNode node x || containNode node y
containNode node (Single x) = x == node


-- Question 2 : " Does this Edge Appear in the Graph"
containEdge :: Edge -> Graph -> Bool
containEdge edge (Union x y z) = (contain z edge) || (containEdge edge x) || (containEdge edge y)
containEdge edge _ = False


edgeEquality :: (Int,Int) -> (Int,Int) -> Bool
edgeEquality x y
            |getFirst x == getFirst y && getSecond x == getSecond y = True
            | otherwise = False

contain :: [(Int,Int)] -> (Int,Int) -> Bool
contain [] edge = False
contain list edge
      | edgeEquality (getHead list) edge == True = True
      | otherwise = contain (getTail list) edge


-- Question 3 : "List All Nodes that Appear in this Graph"
listAllNodes :: Graph -> [Node]
listAllNodes (Union x y z) = (listAllNodes x ++ listAllNodes y)
listAllNodes (Single x) = [x]


-- Question 4: "List All Edge that Appear in this Graph"
listAllEdges :: Graph -> [Edge]
listAllEdges (Union x y z) = (z ++ listAllEdges x ++ listAllEdges y)
listAllEdges (Single x) = []


-- Question 5: "Is this Graph a Singleton"
checkForSingleton :: Graph -> Bool
checkForSingleton graph = checkingSingletonFunction (listAllNodes graph) (listAllEdges graph)
-- checkForSingleton (Union (Single 1)(Union(Single 2)(Union(Single 3)(Single 4)[])[(4, 2)])[ (1, 4), (2, 1)]) (is True because node 3 has no edges)
-- checkForSingleton (Union (Single 1)(Union(Single 2)(Union(Single 3)(Single 4)[])[(3, 2), (4, 2)])[(1, 3), (1, 4), (2, 1)]) (the original one is not a singleton)

nodeHasEdge :: Node -> [Edge] -> Bool
nodeHasEdge node edges
            | edges == [] = False
            | node == getFirst (getHead edges) ||
              node == getSecond (getHead edges)  = True
            | otherwise = nodeHasEdge node (getTail edges)

checkingSingletonFunction :: [Node] -> [Edge] -> Bool
checkingSingletonFunction node_list edge_list
                        | node_list == [] = False
                        | (nodeHasEdge (getHead node_list) edge_list) == False = True
                        | otherwise = checkingSingletonFunction (getTail node_list) edge_list

-- Question 6 : "Compute a Breadth-First Search Traversal"
breadthFirstSearch :: Graph -> Node -> [Node]
breadthFirstSearch graph node
                  | containNode node graph == True = supSearch (sortEdgeFromGraph graph) node
                  | otherwise = error "There is something wrong with your input"

-- breadthFirstSearch (Union (Single 1)(Union(Single 2)(Union(Single 3)(Single 4)[])[(3, 2), (4, 2)])[(1, 3), (1, 4), (2, 1)]) node
-- breadthFirstSearch (Union (Single 1)(Union(Single 2)(Union(Single 3)(Single 4)[])[])[(1, 3), (1, 4), (2, 1),(3, 2), (4, 2)]) node


-- get a sorted [edge] as input (increasing order) and a node
supSearch :: [Edge] -> Node -> [Node]
supSearch  list node
        | list == [] = [node]
        | otherwise = (node : goThroughStep (renewList list node) (getAQueue list node))

-- get a sorted [edge] and a queue as inputs
goThroughStep :: [Edge] -> [Node] -> [Node]
goThroughStep edges nodes
        | edges == [] = nodes
        | otherwise = (getHead nodes : goThroughStep (renewList edges (getHead nodes)) (add (getTail nodes) (getAQueue edges (getHead nodes))))

-- FindAndDeleteEdges staring from a node
renewList :: [Edge] -> Node -> [Edge]
renewList list node
        | list == [] = []
        | getFirst(getHead list) == node || getSecond(getHead list) == node = renewList (getTail list) node
        | otherwise = ((getHead list) : renewList (getTail list) node)
-- renewList (sortEdgeFromGraph (Union (Single 1)(Union(Single 2)(Union(Single 3)(Single 4)[])[(3, 2), (4, 2)])[(1, 3), (1, 4), (2, 1)]))

-- get a queue of nodes from edges.
getAQueue :: [Edge] -> Node -> [Node]
getAQueue [] _ = []
getAQueue (h:t) node
          | t == [] && getFirst h == node = [getSecond h]
          | t == [] && getFirst h /= node = []
          | t== [] = []
          | getFirst h == node = ((getSecond h) : getAQueue t node)
          | otherwise = getAQueue t node

-- sorting edges method
sortEdgeFromGraph :: Graph -> [Edge]
sortEdgeFromGraph graph = sortingListOfEdgeBySecondElement (sortingListOfEdgeByFirstElement (listAllEdges graph))
-- sortEdgeFromGraph (Union (Single 1)(Union(Single 2)(Union(Single 3)(Single 4)[])[(3, 2), (4, 2)])[(1, 3), (1, 4), (2, 1)])

sortingListOfEdgeByFirstElement :: [Edge] -> [Edge]
sortingListOfEdgeByFirstElement [] = []
sortingListOfEdgeByFirstElement (h:t) = (sortingListOfEdgeByFirstElement [x | x<-t, (getFirst x) <= (getFirst h)])
 ++ [h] ++ (sortingListOfEdgeByFirstElement [x | x<-t, (getFirst x)> (getFirst h) ])

sortingListOfEdgeBySecondElement :: [Edge] -> [Edge]
sortingListOfEdgeBySecondElement [] = []
sortingListOfEdgeBySecondElement (h:t) = (sortingListOfEdgeBySecondElement [x | x<-t, (getFirst x) == (getFirst h), (getSecond x) <= (getSecond h)])
 ++ [h] ++ (sortingListOfEdgeBySecondElement [x | x<-t, (getFirst x) ==  (getFirst h), (getSecond x) > (getSecond h) ])
 ++  (sortingListOfEdgeBySecondElement [x | x<-t, (getFirst x) /=  (getFirst h)])


-- Question 7: Create a graph from an Adjacency list
createAGraphFromAList :: [(Node,[Node])] -> Graph
createAGraphFromAList list
                  | getLength (getAllNodes list) == 0 = error "You enter an empty list"
                  | otherwise = generateGraph (getAllNodes list) (getAllEdges list)


generateGraph :: [Node] -> [Edge] -> Graph
generateGraph (h:[]) [] = Single h
generateGraph (h:t) [] = Union (Single h)  (generateGraph t []) []
generateGraph (h1:t1) (h2:t2) = Union (Single h1)  (generateGraph t1 []) (h2:t2)


getAllNodes :: [(Node,[Node])] -> [Node]
getAllNodes [] = []
getAllNodes (h:t) = (getfst h : getAllNodes t)


getAllEdges :: [(Node,[Node])] -> [Edge]
getAllEdges [] = []
getAllEdges (h:t) = (generateEdges h) ++ (getAllEdges t)


generateEdges :: (Node,[Node]) -> [Edge]
generateEdges (first, second)
            | second == [] = []
            | otherwise = ((first, getHead second) : generateEdges (first, getTail second))


-- Question 8: Create a graph from an Adjacency matrix
createAGraphFromAMatrix :: [[Bool]] -> Graph
createAGraphFromAMatrix matrix
                      | getLength matrix == 0  || checkForSquare matrix (getLength matrix) == False = error "It is not a square matrix"
                      | otherwise = generateGraph (createAListOfNodes (getLength matrix)) (createAListOfEdges matrix 1)

createAListOfNodes :: Int -> [Int]
createAListOfNodes 0 = []
createAListOfNodes num = createAListOfNodes (num-1) ++ [num]

createAListOfEdges :: [[Bool]] -> Int -> [Edge]
createAListOfEdges [] node = []
createAListOfEdges (h:t) node = (makeList h node (getLength h)) ++ createAListOfEdges t (node+1)

makeList :: [Bool] -> Int -> Int -> [Edge]
makeList [] _ _ = []
makeList (h:t) node size
                        | h == True = ((node , size - (getLength t)) : makeList t node size)
                        | otherwise = makeList t node size


checkForSquare :: [[Bool]] -> Int ->Bool
checkForSquare [] _ = True
checkForSquare (h:t) size
              | getLength h /= size = False
              | otherwise = checkForSquare t size


--Support Functions

getfst :: (a,[a]) -> a
getfst (a,_) = a

getsnd :: (a,[a]) -> [a]
getsnd (_,[a]) = [a]


getFirst :: (a,a) -> a
getFirst (x,y) = x

getSecond :: (a,a) -> a
getSecond (x,y) = y

getHead :: [a] -> a
getHead [] = error "Can't get head of an empty list"
getHead (x:y) = x

getTail :: [a] -> [a]
getTail [] = []
getTail (x:y) =y

getLength :: [a] -> Int
getLength [] = 0
getLength (_:x) = 1 + length x

doTake :: [a] -> Int -> [a]
doTake list num
    | num == 0 = []
    | otherwise = (head list: doTake list (num-1))

doDrop :: [a] -> Int -> [a]
doDrop list num
      | num == 0 = list
      | otherwise = doDrop (getTail list) (num-1)


doDelete :: [Edge] -> Edge -> [Edge]
doDelete []  a = []
doDelete (h:t) target
        | h == target = t
        | otherwise = (h : doDelete t target)

add :: [Node] -> [Node] -> [Node]
add l1 l2 = l1 ++ [x | x <- l2, containNodeInList l1 x == False]

containNodeInList :: [Node] -> Node -> Bool
containNodeInList [] _ = False
containNodeInList (h:t) node
                  | h== node = True
                  | otherwise = containNodeInList t node


doReverse :: [(Node, [Node])] -> [(Node,[Node])]
doReverse [] = []
doReverse (h:t) =  (doReverse t) ++ [h]
