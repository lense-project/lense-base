# The L.E.N.S.E. base implementation
(Learning from Expensive, Noisy, Slow Experts)

Check out the [project summary](http://lense-project.github.io) to get a rough idea of the setting.

This is intended to be the base implementation that handles all the unpleasant heavy lifting associated with LENSE.

Experimenting with new `GamePlayer` strategies is easy. Below we explain how the CRF machinery works (a wrapper over Factorie)
and then talk about how to subclass `GamePlayer` and run tests.

# Getting Started:

This project was built in IntelliJ, so it is the recommended way to edit.

To get started: Clone the repo, and open it up.

# Understanding The Bits and Pieces

There are a couple of responsibilities in making a CRF-based crowd-machine hybrid classifier. This is the list of 
what they are and where to find them.

## CRF Creation and Inference: `GraphStream` and its associates

CRF creation is handled by `edu.stanford.lense_base.graph.GraphStream`. A `GraphStream` is the mechanism for weight-
sharing between individual **graphs in a stream** (hence the name). You can create a graph as follows:

```
val stream = new GraphStream()
val g = stream.newGraph()
```

In order to populate your CRF, you need **nodes** and **factors**. The `GraphStream` needs to know about what values
your nodes can take, and what neighbors each factor can take. This is handled with the `stream.makeNodeType()` and 
`stream.makeFactorType()` calls. These `NodeType` and `FactorType` objects are then passed to `graph.makeNode()` and
`graph.makeFactor()` to tell the **graph** what weights to share with the **stream**.

To make a simple network with two nodes, which can each take values in {"YES", "NO"}, and with a binary factor between 
them, we do the following:

```
// Create the node type with possible values in set yes-no
val yesNoNodeType = stream.makeNodeType(Set("YES","NO"))
// Create the factor type with two yes-no neighbors
val binaryFactorType = stream.makeFactorType(List(yesNoNodeType, yesNoNodeType))
// Create a pair of actual yes-no nodes on the *graph*
val node1 = g.makeNode(yesNoNodeType)
val node2 = g.makeNode(yesNoNodeType)
val factor = g.makeFactor(binaryFactorType, List(node1,node2))
```

Once you have a graph, you generally want to do two things: first, you want to learn MLE parameters over a set of graphs 
with some observed values. You then also want to use those learned weights to perform marginal inference and MAP
inference on your graphs. 

To learn MLE estimates of model weights, we set `node.observedValue = ...` for all (or some, for EM) of the nodes in a set
of graphs. We then call `stream.learn()` and it will learn MLE estimates of the weights. You can pass in L2 regularization
to `stream.learn()`. For the running example of a pair of YES-NO nodes, to learn weights for the stream from a single 
graph, we would do:

```
node1.observedValue = "YES"
node2.observedValue = "NO"
stream.learn(List(g))
```

All newly created graphs on the stream that use `yesNoNodeType` and `binaryFactorType` will share weights with their type-mates, 
and we just set those weights with a call to `stream.learn()`. To use the weights for some useful purpose, we can do **inference**.
Marginal inference is `g.marginalEstimate()`, and MAP inference is `g.mapEstimate()`. Marginal estimates return a `Map[GraphNode,Map[String,Double]]`
where each node corresponds to a map, where the keys are assignments and the values are probabilities. Map assignments return
a `Map[GraphNode,String]`, where each node just corresponds with its global MAP assignment.

## Central Coordination: `LenseEngine` and `LenseUseCase`

`LenseEngine` handles all the core logic of running an algorithm, where the `GraphStream` is given at creation, and 
`engine.predict()` expects a graph from that `GraphStream`, which it will then return a MAP estimate for, after querying
humans "as necessary" (see the next section for how this is determined).

`LenseEngine` is a very low-level interface, so to help abstract away commonly reimplemented stuff, `LenseUseCase` and its
subclasses `LenseSequenceUseCase` and `LenseMulticlassUseCase` provide nice abstract interfaces that can be subclassed to
implement individual use cases. `LenseUseCase` contains logic for doing **lots of analysis** in a way that can be shared
across use cases.

`LenseUseCase` receives lots of configuration, but the key thing is specifying which `GamePlayer` to use. This is done in
`LenseUseCase` subclasses with the following code:

```
override def gamePlayer = ...
```

## Decision Making: `GamePlayer`, and `GameState`, and `GameMove`

Given that a user hands us a `Graph` from a `GraphStream`, how do we decide how to optimally behave? This is up to the
`GamePlayer`, which is responsible for implementing `getOptimalMove(state : GameState) : GameMove`.

There are three example `GamePlayer`'s implemented already, which live in the `edu.stanford.lense_base.gameplaying` package.
They are:

- `LookaheadOneHeuristic`: Looks at all outcomes of a single move, not taking into account any currently in-flight requests,
and returns the one with the least loss.
- `NQuestionBaseline`: Asks N questions per node, then turns in the result.
- `ThresholdHeuristic`: Looks at the current marginals, and if confidence is less than some value, sends another query. This improves
confidence for queries in flight using a stupid heuristic, so it is able to handle asynchrony.

**In order to test a `GamePlayer` object**:
go into a prepared use case in `edu.stanford.lense_base.examples`, and override `gamePlayer`,
then run its analysis code (uncomment the test you're interested in in the main method, then right click on the object in the 
file explorer and click "Run" in IntelliJ).
