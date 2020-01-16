
[//]: (# gvim:  fileencoding=utf8)

# Some Recursive Search Engines and Four Solved Problems

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

The contents of this directory cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

## HISTORY

   * January 2019:  first version
   * March 2019:  Reorganisation of the whole directory
   * June 2019:  Writing of this readme file

## DIRECTORY OVERVIEW

This directory contains:
   * several (recursive) inference engines (or solvers), with and without tracing:
        * naïve,
        * with directed-cycle detection,
        * with depth limit,
        * with iterated deepening,
        * with _uninformed_ branch-and-bound,
        * with _informed_ branch-and-bound _and_ directed-cycle detection,
        * with memoïng (or memoïsing, or memorising), and
   * four problems to solve:
        * the bridge crossing riddle,
        * the (improved) crossing frogs,
        * the filling jugs, and
        * the puzzles 8 and 15.

## SOLVERS

There could be a single solver!
However, rather than incorporating all the requirements into a single module, this library emphasises the search needs of problems by providing more and more complex solvers.

Here we limit the presentation to recursive search engines.
They present the main advantage to limit the space to a candidate solution path rather than storing all the reached state nodes, which is, generally, an exponential space...

Let us introduce the provided inference engines in increasing order of complexity.

### Naïve

The naïve search engine is a _very basic_ backtracking solver...

It can be safely used _only_ for
   * (i) _finite_, and
   * (ii) _directed acyclic_
state graphs.

This engine closely mimics the way Prolog solves a problem.
Hence it is generally not to be used!
In particular, it founds _all_ the solutions, including the ones that are extensions of a previous one.

### Directed-cycle Detection

A backtracking solver with directed cycle detection slightly extends the naïve version.

Its goal is to avoid one of its two main problems, namely searching round and round into infinite directed cycles!
This is achieved by keeping a record of the path from the initial state to the current one.
Whenever a new node is reached, the engine checks if this node has already been found, i.e., is present in the path from the initial state.
If it is so, then we have detected a directed cycle, hence the search along this branch fails immediately.
The cost of performing this check is extremely low with respect to the reduction of the size of the search tree.

Notice that this extension is sufficient for _finite_ state graphs.
However, it remains inefficient when the graph contains a lot of _undirected_ cycles, but this remains a big problem for any kind of solver...

### Depth-limit

A backtracking solver with (default) depth limit is another simple extension of the naïve version.

A depth limit argument is added in order to avoid infinite searches into infinite state sub-graphs.
This depth limit is decreased for each successor of a given node.
Should it reach zero without finding a final state, the search is declared unfruitful.

This algorithm can be used both for infinite state graphs and graphs with directed cycles.
In other words, it can be used effectively to conduct searches on any kind of state graph!
It is the first effective version.

However, it is not expected to be efficient.
In addition, should the depth limit be chosen too small, it will miss solutions, so it is not complete.

### Iterated-deepening

The iterated-deepening version should be a meta-algorithm.
Here, it relies directly on the previously introduced depth-limit search algorithm.
It could have been applied to any kind of algorithm accepting a depth or cost limit.

This iterated-deepening (meta) solver (with default maximal depth limit) tries to circumvent the main limitation of the depth-limit solver.
In the latter, the depth limit is chosen _in advance_, which can turn out to be difficult:
   * Should it be _too small_, we shall miss solutions;
   * Should it be _too large_, we shall waste a lot of time in large and unproductive branches.
Here, we start with a minimal depth-limit of only one but increment it whenever no solution has been found.
However, in order to avoid supposedly infinite searches (or a least too expensive ones), a maximal depth can be given.
(A rather large one is provided as a default value.)

This solver is _both_:
   * effective, and
   * _rather_ efficient.

Basically, it repeats searches at deeper and deeper levels in order to find a solution.
Therefore, it is a kind of breadth-first search.
Hence, it is _optimal_ with respect to the _length_ of the solution (not necessarily its cost).
Also, it is _complete_ as long as the state graph is locally finite (i.e., no operation generates an infinity of successor states).

Finally, its asymptotic time complexity is the same as the underlying depth-limit search.
Effectively, running iteratively an exponential algorithm at most doubles its time complexity, which is a very small multiplicative constant!

All in all, this (meta) solver is often the best choice, especially when the structure of the search space has not yet been studied.

### Uninformed Branch-and-bound

Branch-and-bound is a backtracking technique, the goal of which is to find a _best_ solution with respect to a cost function, not the length as it is the case with iterated-deepening.
The main difference with respect to the previous algorithm is that it can find at most one of the best solutions, not even all of them, not to mention less interesting ones.

The definition of this algorithm is quite convoluted.
The reader can have a look at the module documentation.

__Nota.__  This variant provides a _minimisation_.
A maximisation problem can be translated into a minimisation one by negating the scores.

__Nota.__  The implementation restricts the score function to integer values.

### Informed Branch-and-bound

Branch-and-bound is not really efficient on _uninformed_ searches.
Indeed, the actual definition of this algorithm incorporates a heuristic in the form of an evaluation function.
This heuristic function is able to guide the search process in the best direction.
Notice that this property is _primordial_ for a recursive search.
Whenever the search enters a sub-optimal branch, most of this sub-graph will be explored to no advantage.
The ideal situation is to recursively choose the best branch as the first successor to explore, hence going directly to the solution!
This ideal behaviour is to be expected at least at the higher levels of the search tree.

__Nota.__  This version is combined with a directed-cycle detection.

### Memoïng

Contrary to what has been announced in the introduction of this section, we also provide a memory-consuming solver, a variant that, indirectly, stores _all_ the found states!
This variant offers a trade-off between memory consumption and _super_ exponential search reduction.
Effectively, when a graph has a lot of cycles, sub-graphs can be expanded into tree branches again and again.
So, this costly version can be unavoidable for some kinds of problems.

__Nota.__  This version only memoïses solutions.
It would be necessary to extend it to memoïng failures too, in order to avoid super-exponential searches on sterile sub-graphs!

__Nota.__  This version fills up the memory as much as possible.
It would be necessary to extend it with some memory management, e.g., LRU removal, in order to make it runnable on any instance.

### Missing Combinations and Extensions

Not all the requirements of problems are answered in this directory.

An interesting and easy extension is to combine complementary techniques.
Most easily, in order to improve the search time in graphs containing a lot of directed cycles, it is recommended to combine the directed-cycle detection with a depth limit:
   * The latter uses as an argument a path from the initial state, and a membership test to find out a _certain_ failure.
   * The former uses as an argument a depth limit, and a zero threshold test to find out a _possible_ failure.
   * Their combination easily uses both arguments and tests.
In turn, these two controls can be added to iterated-deepening.

In contrast, the cost limit of branch-and-bound generalises the role of a depth limit.
So, the combination with directed-cycle detection is enough.

With respect to memoïng, the directed-cycle detection has also been incorporated.
However, it could have been considered a third kind of memoïng:  associating to a newly reached state an unknown solution (in addition to a known solution -- as currently implemented -- or a known _absence_ of solution -- as recommended).

An important extension is to introduce heuristics in the various algorithms.

This is of special interest for the branch-and-bound version, the overall goal of which is to find a _best_ solution.
Generally, this can only be achieved thanks to an evaluation function.
In fact, the normally _informed_ branch-and-bound algorithm is the backtracking counterpart of A*.
In contrast, the provided uninformed version is not even the counterpart of a best-first search!
However, a recursive best-first search variant could be achieved by adapting the iterated-deepening version to a cost model rather than using only the depth.

## EXAMPLES

Some problems are provided and solved with one or another of the developed solvers.

### Bridge Crossing Problem

The bridge problem has a very simple state graph structure.
It can be solved with the naïve solver, i.e., basically, directly in Prolog.

__Nota.__  It belongs to a large family of related problem, e.g., the goat, lettuce, wolf, and farmer, where the goal is to cross an obstacle under some constraints.

### Crossing Frogs Problem

This is a variation of the previous family of problems.
It can also be solved with the naïve solver.

### Filling Jugs Problem

The jugs problem is related to the higher class of planning problems.
Both its state graph is _infinite_ and a meaningful cost can be associated to a solution.
However, it is hard to come with a heuristic to guide the search...
Therefore, all the solvers, except the informed one are used.
(Here, the naïve one has some issues!)

### Puzzle 8 and 15 Problems

Puzzle 8 and 15 (a.k.a. "Taquin" in French) is a standard problem in AI.
Puzzle 8 is played on a 3 x 3 grid, whereas puzzle 15 is played on a large grid of size 4 x 4.
It highly benefits from an informed (heuristic) search.

## LIBRARY USE

### New Problem Description

Basically, when one wants to solve a new problem, say NEW, one needs to:
   * create a 'NEW_problem.pl' file with three mandatory predicates:
        * 'initial_state/1',
        * 'final_state/1', and
        * 'operation/4',
   * create either one 'NEW_solution.pl' file or several files 'NEW_solution_IE.pl', where IE stands for one of the numerous inference engines.

### New Problem Solver Choice

Due to the lack of genericity, each 'NEW_solution_IE.pl' file is based on the very same pattern:
   * import the 'NEW_problem.pl' module,
   * import the 'solver_common.pl' module,
   * import the corresponding 'IE.pl' module,
   * write down the predicate 'solve/2' that is limited to:
        * finding an initial problem instance with 'initial_state/1' from 'NEW_problem.pl',
        * solving it with 'solver/4', 'solver/5', 'solver/6', or 'solver/7' from 'IE.pl',
        * displaying the solution with 'pretty_solution/2' from 'solver_common.pl', and/or enhancing it.

Regardless of the boiler-plate code, the role of these modules is to solve the same problem with different search engines.
Ideally, only the best-fitted one is to be written down.
Its choice is based on the structure of the problem:
   * Is the graph infinite?
   * Does it contain directed cycles?
   * Does it contain undirected cycles?
   * Are we looking for one solution?
   * Are we looking for a best solution?
   * Etc.

The dependencies between modules (more precisely module instantiations) is as follows, on two problems:

==
                       +--------------------------+--------+
                       |                          |        |
   +---------------------+       +------------------+      |
   | frogs_solution_IE   |       | jugs_solution_IE |      |
   +---------------------+       +------------------+      |
   | solve/0             |       | solve/0          |      |
   | solve_and_animate/0 |       | solve/0          |      |
   +---------------------+       +------------------+      |
             |                             |               |
             |                             |               |
             V                             V               |
+--------------------------+  +------------------------+   |
| frogs_problem            |  | jugs_problem           |   |
+--------------------------+  +------------------------+   |
| initial_state/1          |  | initial_state/1        |   |
| final_state/1            |  | final_state/1          |   |
| operation/4              |  | operation/4            |   |
|                          |  |                        |   |
| frogs_pretty_solution/2  |  | jugs_pretty_solution/2 |   |
| frogs_pretty_animation/2 |  +------------------------+   |
+--------------------------+                               |
                                                           |
                                                           | IE
                                                           |
                         +-----------------------------+   |
                         | solver_naive(_with_tracing) |<--+
                         +-----------------------------+   |
                         | solver/4                    |   |
                         +-----------------------------+   |
                                                           |
                                                           |
      +------------------------------------------------+   |
      | solver_directed_cycle_detection(_with_tracing) |<--+
      +------------------------------------------------+   |
      | solver/4                                       |   |
      +------------------------------------------------+   |
                                                           |
                   +-----------------------------------+   |
                   | solver_depth_limit(_with_tracing) |<--+
                   +-----------------------------------+   |
                   | solver/4                          |   |
                   | solver/6                          |   |
                   +-----------------------------------+   |
                                          ^                |
                                          |                |
                                          |                |
            +------------------------------------------+   |
            | solver_iterated_deepening(_with_tracing) |<--+
            +------------------------------------------+   |
            | solver/4                                 |   |
            | solver/5                                 |   |
            +------------------------------------------+   |
                                                           |
                  +------------------------------------+   |
                  | solver_uninformed_branch_and_bound |<--+
                  +------------------------------------+   |
                  | solver/5                           |   |
                  +------------------------------------+   |
                                                           |
                             +-------------------------+   |
                             | solver_branch_and_bound |<--+
                             +-------------------------+   |
                             | solver/5                |   |
                             +-------------------------+   |
                                                           |
                                            +----------+   |
                                            | solver   |<--+
                                            +----------+   |
                                            | solver/5 |   |
                                            +----------+   |
                                                           |
                                      +----------------+   |
                                      | solver_memoing |<--+
                                      +----------------+
                                      | solver/4       |
                                      +----------------+

==

where arrows represent 'use_module'.

