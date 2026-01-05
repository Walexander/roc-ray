module [
    Graph,
    astar3,
    fromList,
    fromDict,
    aStar,
    astar,
]
import PriorityQueue
## Graph type representing a graph as a dictionary of adjacency lists,
## where each key is a vertex and each value is a list of its adjacent vertices.
Graph a := Dict a (List a) where a implements Eq

Graph2 a : a -> Result (List a) [NotFound] where a implements Eq

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

make_path_to : a, Dict a a -> List a
make_path_to = |element, parents|
    iter = \child, accum ->
        Dict.get parents child
        |> Result.map_ok \child_ -> iter child_ List.prepend(accum, child_)
        |> Result.with_default accum
    iter element [element]


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
astar3 = \{ isTarget, estimator, cost_fn, graph, root } ->
    ## this processes neighbors of currentNode, updating the cost dictionary, parent node and frontier
    step = |neighbors, currentNode, frontier, costs, parents|
        currentCost =
            Dict.get costs currentNode
            ## If we dont have a cost for our current node, shit is really fux0red
            |> Result.map_err |_| crash "how can we be missing a cost to the current node??"
            |> Result.with_default 100

        ## map our neighbors, only including nodes we haven't seen before
        ## or, if we have seen them, if the cost to get there from current node is
        ## cheaper than the one we saw previously
        ## for the neighbors we want, return a tuple of (cost, node)
        List.keep_oks(neighbors, \node ->
            new_cost = currentCost + cost_fn(currentNode, node)
            when Dict.get costs node  is
                Err _ -> Ok (new_cost, node)                                               ## Keep: not seen yet
                Ok curr_cost if curr_cost > new_cost ->
                    # dbg "cheaper path to ${Inspect.to_str node} through ${Inspect.to_str currentNode} ${Inspect.to_str curr_cost} > ${Inspect.to_str new_cost}"
                    Ok (new_cost, node)                                                    ## Keep: cheaper to go through currentNode
                Ok _ -> Err KeyNotFound                                                    ## Skip: we've seen it before and the existing path is cheaper
        )
        ## now walk over the valid neighbors
        |> List.walk({ costs, parents, stack: frontier },
            |accum, (node_cost, node)| {
                costs: Dict.insert accum.costs node node_cost,                                 ## upsert cost to traverse this node
                parents: Dict.insert accum.parents node currentNode,                           ## upsert the neighbor node's parent
                stack: PriorityQueue.push accum.stack (node, node_cost + estimator(node))      ## add our current cost + our a* heuristic to generate a new priority
            })

    ## recursive function that processes the next step of a frontier
    ## returns when our stack is empty or we found the node we're looking for
    helper = |frontier, current_costs, parent_paths|
        ## get the next node from our stack
        PriorityQueue.pop frontier
        |> Result.try \((currentNode, _), next_stack) ->

            if isTarget currentNode then                                                  ### We found it!!
                ## this *is* our node, return it along with the parent paths
                Ok (currentNode, parent_paths)
            else
                ## this isnt our node, so get its neighbors
                graph currentNode
                |> step currentNode next_stack current_costs parent_paths
                |> |{stack, costs, parents }| helper stack costs parents
        ## we popped an empty frontier
        ## we've run out of nodes and haven't found our target
        |> Result.map_err |_| NotFound

    initialCosts = Dict.empty {} |> Dict.insert root 0.0
    initialParents_ = Dict.empty {}

    comparator = \(_, a), (_, b) -> Num.compare a b
    initialStack = PriorityQueue.make comparator |> PriorityQueue.push (root, 0)

    helper initialStack initialCosts initialParents_
    |> Result.map_ok  \(t, paths) -> (t, make_path_to t paths)
