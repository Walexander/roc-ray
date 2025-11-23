module [
    Graph,
    astar3,
    fromList,
    fromDict,
    aStar,
    astar,
]
import PriorityQueue exposing [PriorityQueue]
## Graph type representing a graph as a dictionary of adjacency lists,
## where each key is a vertex and each value is a list of its adjacent vertices.
Graph a := Dict a (List a) where a implements Eq

Graph2 a : a -> Result (List a) [NotFound] where a implements Eq

fromList2 : List (a, List a) -> Graph2 a
fromList2 = \adjacencyList ->
    \a ->
        List.find_first adjacencyList \(b, _) -> b == a
        |> Result.map_ok  \tuple -> tuple.1
        |> Result.map_err \_ -> NotFound

## Create a Graph from an adjacency list.
fromList : List (a, List a) -> Graph a
fromList = \adjacencyList ->
    emptyDict = Dict.with_capacity (List.len adjacencyList)

    update = \dict, (vertex, edges) ->
        Dict.insert dict vertex edges

    List.walk adjacencyList emptyDict update
    |> @Graph

## Create a Graph from an adjacency list.
fromDict : Dict a (List a) -> Graph a
fromDict = @Graph

Estimator a : a -> I32
Compare a : a, a -> [LT, GT, EQ]
constZero : Estimator a
constZero = \_ -> 0
# prioritizeStack : Compare (a, I32)
# prioritizeStack = \a, b ->
#     aa = a.1 #estimator a.1 |> Num.add a.1
#     bb = b.1 #estimator b.1 |> Num.add b.1
#     if aa < bb then
#         LT
#     else if aa > bb then
#         GT
#     else
#         EQ
priorityWithEstimate : Estimator a -> Compare (a, I32)
priorityWithEstimate = \estimator -> \a, b ->
        aa = estimator a.0 |> Num.add a.1
        bb = estimator b.0 |> Num.add b.1
        if aa < bb then
            LT
        else if aa > bb then
            GT
        else
            EQ

## priorityWithEstimate should return natural order when cost is always 0
expect
    sortFn = priorityWithEstimate constZero
    list = [("a", 2), ("b", 0), ("c", 1)]
    actual = List.sort_with list sortFn
    expected = [("b", 0), ("c", 1), ("a", 2)]
    actual == expected

## priorityWithEstimate should compare current + estimated value
## when sorting
expect
    sortFn = priorityWithEstimate \s -> if Str.starts_with s "b" then 5 else 0
    list = [("a", 2), ("b", 0), ("c", 1)]
    actual = List.sort_with list sortFn
    expected = [("c", 1), ("a", 2), ("b", 0)]
    actual == expected

CostDict a : List (a, I32)
findCost : a, CostDict a -> Result (a, I32) [NotFound] where a implements Hash & Eq
findCost = \a, costs ->
    List.find_first costs \(n, _) -> n == a
    |> Result.map_ok  .1
    |> Result.map_ok  \cost -> (a, cost)
    |> Result.map_err \_ -> NotFound

addCosts = \nodes, currentCost, costs ->
    List.walk nodes costs \accum, discovered ->
        insertCost discovered (currentCost + 1) accum

insertCost = \a, cost, costs -> List.append costs (a, cost)

makeCosts : a -> CostDict a where a implements Hash & Eq
makeCosts = \a -> [(a, 0)]
# List.with_capacity 32 |> List.set 0 (a, 0)

# [(a, 0)] |> List.reserveCapacity 32
# Parents a : List (parent, child)
Parents a : List (a, a) where a implements Hash & Eq
findParentOf : a, Parents a -> Result a [NotFound] where a implements Eq & Hash
findParentOf = \a, parents ->
    # Dict.get parents a
    List.find_first parents \(_, child) -> child == a
    |> Result.map_ok  .0
    |> Result.map_err \_ -> NotFound

addParents : a, List a, Parents a -> Parents a
addParents = \current, nodes, parents ->
    # List.map nodes \c -> (current, c)
    # |> List.concat current
    List.walk nodes parents \accum, discovered ->
        insertParent discovered current accum

insertParent = \child, parent, parents -> List.append parents (parent, child)

makeEmptyParents : _ -> Parents a where a implements Hash & Eq
makeEmptyParents = \_ -> List.with_capacity 64

## Takes our Parents and a target node and returns a list of target nodes
## in order
makePathTo : a, Parents a -> List a
makePathTo = \v, paths ->
    iter = \p, ps ->
        findParentOf p paths
        |> Result.map_ok  \a -> iter a (List.prepend ps a)
        |> Result.with_default ps
    # iter v ([v] |> List.reserve 128)
    iter v [v]

## Perform a breadth-first search with a fixed cost of 1 for each step
## and an `Estimator` function to determine priority
## - `isTarget` : A function that returns true if a vertex is the target.
## - `root`     : The starting vertex for the search.
## - `graph`    : The graph to perform the search on.
aStar : (a -> Bool),
    Estimator a,
    a,
    Graph2 a
    ->
    Result (a, List a) [NotFound] where a implements Hash & Eq & Inspect
aStar = \isTarget, estimator, root, graph ->
    initialCosts = makeCosts root
    initialParents = makeEmptyParents {}
    stack = [root] # |> List.reserve 128
    sorter = priorityWithEstimate estimator
    aStarHelper isTarget sorter stack initialCosts initialParents graph
    |> Result.map_ok  \(t, paths) -> (t, makePathTo t paths)

astar : { isTarget : a -> Bool, root : a, graph : Graph2 a, estimator : Estimator a }
    ->
    Result (a, List a) [NotFound] where a implements Hash & Eq & Inspect
astar = \{ isTarget, root, estimator, graph } ->
    aStar isTarget estimator root graph

# aStarHelper does not die
# expect
#     dict = makeCosts "A"
#     sorter = priorityWithEstimate constZero
#     actual = aStarHelper (\_ -> Bool.true) sorter ["A"] dict (makeEmptyParents {}) testGraph2
#     Result.isOk actual

## aStar terminates with empty graph
# expect
#     actual = astar3 {
#         isTarget: \_ -> Bool.false,
#         estimator: constZero,
#         root: "A",
#         graph: emptyGraph,
#     }
#     expected = Err NotFound
#     actual == expected

# ## aStar It terminates when target not found
# expect
#     actual = astar3 {
#         isTarget: \_ -> Bool.false,
#         estimator: constZero,
#         root: "A",
#         graph: testGraph2,
#     }
#     expected = Err NotFound
#     actual == expected

# ## aStar finds the one starting with "C"
# expect
#     actual = Result.map_ok
#         (
#             astar3 {
#                 isTarget: \v -> Str.starts_with v "C",
#                 estimator: constZero,
#                 root: "A",
#                 graph: testGraph2,
#             }
#         )
#         .0

#     expected = Ok "Ccorrect"

#     actual == expected

# expect
#     actual =
#         astar3 {
#             isTarget: \v -> Str.starts_with v "C",
#             estimator: constZero,
#             root: "A",
#             graph: testGraph2,
#         }
#         |> Result.map_ok  .0

#     expected = Ok "Ccorrect"

#     actual == expected

# # ## aStar finds the one starting with "B"
# expect
#     actual =
#         astar3 {
#             isTarget: \v -> Str.starts_with v "B",
#             estimator: constZero,
#             root: "A",
#             graph: testGraph2,
#         }
#     expected = Ok ("B", ["A", "B"])
#     actual == expected

# A helper function for performing A* search.
#
# `isTarget`   : A function that returns true if a vertex is the target.
# `estimator`  : An estimator functions that cacluclates approx distance to target.
# `stack`      : List of vertices remaining.
# `costs`      : CostDict for looking up the cost to reach each node's parent
# `parents`    : Parents object for tracking each node's parent
# `graph`      : The graph to perform the search on.
aStarHelper : (a -> Bool),
    Compare (a, I32),
    List a,
    CostDict a,
    Parents a,
    Graph2 a
    ->
    Result (a, Parents a) [NotFound]
    where a implements Hash & Eq & Inspect
aStarHelper = \isTarget, sorter, stack, costs, parents, graph ->
    when stack is
        # we have run out of nodes, Err
        # but We're done!
        [] -> Err NotFound
        # otherwise, take the first node and rest of our stack
        [current, .. as rest] ->
            if isTarget current then
                Ok (current, parents) ## Yay, we found it
            else
                # get the cost to our current node
                currentCost =
                    findCost current costs
                    |> Result.map_ok  .1
                    |> Result.with_default 0
                # expand the current node neighbors
                when graph current is
                    Ok neighbors ->
                        # step with our current neighbor list
                        next = aStarStep neighbors rest current currentCost costs parents sorter
                        # and recurse with updated values
                        aStarHelper
                            isTarget
                            sorter
                            next.stack
                            next.costs
                            next.parents
                            graph

                    Err _ ->
                        # no neighbors, keep going
                        aStarHelper isTarget sorter rest costs parents graph

aStarStep = \neighbors, rest, current, currentCost, costs, parents, sorter ->
    newbies =
        neighbors # discard the nodes we have already *seen*
        |> List.keep_if (\n -> Result.is_err (findCost n costs))
    newCosts = addCosts newbies currentCost costs
    stack = List.concat rest newbies
        |> List.keep_oks \a -> findCost a newCosts
        |> List.sort_with sorter
    {
        costs: newCosts,
        parents: addParents current newbies parents,
        stack: stack |> List.map .0,
    }
## aStar finds shortest path to "C"
# ## It finds the shortest path
constOne = |_, _| 1

astar3 = \{ isTarget, estimator, cost_fn, graph, root } ->
    step = \neighbors, currentNode, nextStack, costs, parents ->
        currentCost =
            Dict.get costs currentNode
            |> Result.with_default 0

        neighbors
        |> List.keep_if (|n|
            when Dict.get costs n is
                Err _ -> Bool.true
                Ok node_cost -> node_cost > currentCost
        )
        |> |newbies|
            # addCosts newbies currentCost costs
            List.walk(newbies, costs, |tmp_costs, node|
                node_cost: I32
                node_cost = cost_fn currentNode node
                Dict.insert tmp_costs node (node_cost)
            )
            |> |newCosts| {
                costs: newCosts,
                parents: addParents currentNode newbies parents,
                stack: List.map newbies |node|
                    (node, currentCost + (estimator node))
                |> List.walk nextStack \accum, value -> PriorityQueue.push accum value,
            }
    aStarHelper3 = |thisStack, costs, parents|
        PriorityQueue.pop thisStack
        |> Result.try \((currentNode, _), nextStack) ->
            if isTarget currentNode then
                Ok (currentNode, parents)
            else
                when graph currentNode is
                    Err _ -> aStarHelper3 nextStack costs parents
                    Ok neighbors ->
                        step neighbors currentNode nextStack costs parents
                        |> |stepResult| aStarHelper3 stepResult.stack stepResult.costs stepResult.parents
        |> Result.map_err |_| NotFound

    initialCosts = Dict.empty {} |> Dict.insert root 0
    initialParents = makeEmptyParents {}

    comparator = \(_, a), (_, b) -> Num.compare a b
    initialStack = PriorityQueue.make comparator |> PriorityQueue.push (root, 0)

    aStarHelper3 initialStack initialCosts initialParents
    |> Result.map_ok  \(t, paths) -> (t, makePathTo t paths)

testGraphMultipath =
    [
        ("A", ["D", "C", "B"]),
        ("C", ["D", "E", "F"]),
        ("D", ["H", "I", "J"]),
        ("B", ["XYZ"]),
        ("H", ["XYZ"]),
        ("I", []),
        ("J", []),
        ("XYZ", []),
    ]
    |> fromList2
emptyGraph = [] |> fromList2
testGraph2 =
    [
        ("A", ["B", "Ccorrect"]),
        ("B", ["D", "Ccorrect", "Cwrong"]),
        ("D", []),
        ("Ccorrect", []),
        ("Cwrong", []),
    ]
    |> fromList2
