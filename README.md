# The L.E.N.S.E. base implementation
(Learning from Expensive, Noisy, Slow Experts)

Check out the [project summary](http://lense-project.github.io) to get a rough idea of the setting.

This is intended to be the base implementation that handles all the unpleasant heavy lifting associated with LENSE.

# Project Goals:

This project is intended to make two things very easy:

- Trying out new game-playing strategies (see `GamePlayer`)
- Applying the techniques to new problems (see `LenseUseCase`)

# Getting Started:

This project was built in IntelliJ, so it is the recommended way to edit.

To get started: Clone the repo, and open it up.

# Running Something:

There are examples under `edu.stanford.lense_base.examples`. The best one (imho) right now is the `NERUseCase`. If you open up
this file in your editor, and look at the `main` method (down at the bottom of the file in the `object NERUseCase` definition)
you see a list of commented `nerUseCase.testWith...` calls. Pick one that looks interesting, uncomment it, and comment out the rest.
Right click on the `NERUseCase` object in IntelliJ and click "Run". This will boot up the system. If you choose `testWithRealHumans`
an embedded Jetty instance will start up, and you can visit the human annotation page at `http://localhost:8080`. Intermediate
results will be printed every couple of examples to `results/ner/*`, with lots of pretty graphs showing different metrics over time,
and tuning plots taken periodically over examples already answered under `results/ner/tuning`.

Feel free to try the other examples, or write your own. Be sure to make sure you data doesn't come in lots of tiny files (`cat $(ls) > merged.txt`
can come in handy for that) and then check it in under `data/`.

# Understanding The Bits and Pieces:

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

**In order to test a (new) `GamePlayer` object**:
go into a prepared use case in `edu.stanford.lense_base.examples`, and override `gamePlayer`,
then run its analysis code (uncomment the test you're interested in in the main method, then right click on the object in the 
file explorer and click "Run" in IntelliJ).

### Understanding `GameState` and `GameMove`:

Before writing your own `GamePlayer`, it's important to understand `GameState` and `GameMove`. 

`GameState` is a complicated object, to provide as much information as possible to game players. Don't worry about most
of the details. The 3 things to pay attention to are 

- `state.graph`, which is a `Graph` object with **extra nodes representing observed 
human responses IN THE GRAPH so they change marginals.** For the original version without extra nodes see `state.originalGraph`, and for a mapping between original and new graph nodes see `state.oldToNew`
- `state.inFlightRequests`, which lists the currently in flight requests, who they're for, and a package `WorkUnit` containing some details
- `state.loss(hypotheticalExtraDelay : Long = 0) : Double`. which returns the loss if the state were turned in with extra delay (in MS) of `hypotheticalExtraDelay`.

`GameMove` is an abstract superclass to 3 Scala case classes that represent
the moves available to a system:

- `case class MakeHumanObservation(node : GraphNode, hcu : HumanComputeUnit)`: This sends out a request to the HCU `hcu`,
asking about the node `node`. The immediate effect will be to launch an in-flight request, which will show up in the state
next time you make a decision.
- `case class TurnInGuess()`: This turns in the guess immediately
- `case class Wait()`: This waits until the next wake-up operation.

### The Gameplay Loop:

In implementation, game-play proceeds as follows: the `LenseEngine` receives a `predict()` request. This creates an initial
`GameState`, and passes it to the `GamePlayer` to find the optimal move. If this move changes the state, but doesn't turn it
in (ie if the move is `MakeHumanObservation`) then the `GamePlayer` is immediately shown the new state with one in-flight 
request, and asked to make a new prediction. This initial flurry of requests continues until the `GamePlayer` decides to 
either `TurnInGuess` or `Wait`. If the `GamePlayer` decides to wait, it is put to sleep, and not asked about what to do
until the state changes again.

Whenever an in-flight request returns or fails (timeout, connection broken, etc) then the `GamePlayer` is woken up again
and asked how to proceed. Any moves the `GamePlayer` makes that change state will lead to more questions for the `GamePlayer`,
until it decides to `Wait` again.

Play terminates when the `TurnInGuess` move is returned, at which point the `GamePlayer` makes some unseen loss, adds a
record of this game to its pile of learning examples, kicks off an asynchronous model retrain (if one isn't already in progress)
and waits patiently for the next example.

### Writing a new `GamePlayer`:

To write a new `GamePlayer` object, there are several useful utilities to be aware of. First, `getAllLegalMoves()` is a method
you get out of the box by subclassing `GamePlayer`. **Use it** when determining what you can and can't do. It handles 
**nasty sychronization issues with budget constraints that you shouldn't touch**. It returns a list of all possible `GameMove`
objects that you could legally return. It will reserve enough budget to allow you to perform any of the moves without going over
budget, and will make sure that you don't query the same person twice.

Also, `state.getNextStates(move)` will return the list of `GameState`s that could result from you taking move `move`. It
will return a list of how humans could respond and **not asynchronous intermediate states**. Use it at your peril.

**Keenon: I am seriously considering changing the behavior of getNextStates() to reflect asynchronous states instead**

# Notes on Running On Mechanical Turk:

IFrames in Mechanical Turk require SSL, so you actually need a certificate signed by a CA, which is a total pain in the
butt and costs money. If you have one, and can point the link in `src/main/resources/mturk/external.question` to your server's IP,
and then change your MTurk config to point to the live Turk servers (if you haven't already) and run a `testWithRealHumans` on
one of the use cases. This will automatically submit a Turk Task, and will wait for at least two Turkers to read instructions
and get ready before kicking off a run through your data batch, which will print logs just like any simulated run.
