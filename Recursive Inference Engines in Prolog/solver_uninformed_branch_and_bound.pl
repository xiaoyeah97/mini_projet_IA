% gvim:  fileencoding=utf8

/** <module> A Backtracking Inference Engine with Uninformed Branch-and-Bound

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * 2014:  first version
   * January 2019:  modularisation;  documentation improvement
   * March 2019:  higher-order predicates;  especially folding in order to clarify the rather complex processing logic of the branch-and-bound strategy

__USAGE__
  
To be imported by a problem (solution) module.

 */

:- module(solver_uninformed_branch_and_bound, [ solver/4
                                              , solver/5 ]).
:- meta_predicate solver(1, 4, ?, ?).
:- meta_predicate solver(1, 4, ?, ?, ?).

:- use_module(solver_common, [ solution_cost/2 ]).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is semidet.
%
% Backtracking solver incorporating a branch-and-bound strategy for optimisation problems, more specifically _minimisation_ ones.
%
% __Nota.__ A maximisation problem can be translated into a minimisation one by negating the scores.
%
% This is the most general version.
% It simply calls its explicit counterpart, 'solver/5', by initialising the upper bound to positive _integer_ infinity.
%
% __Nota.__ Using infinity, even restricted to integers here, may turn out finding the very first solution a _very_ long process.
%           Furthermore, in the common case where the state graph contains directed cycles, the solver will enter and re-enter any loop until the bound be exhausted.
%           The higher the bound, the longer it will take to get out of a loop.
%           In fact, it is most likely that the system will reach an "out of stack" situation long before exhausting the bound!
%           Therefore, it is generally wise to provide a problem-specific upper bound rather than to rely on too high a bound.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   solver(F, O, I, +1Inf, SS).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, +UB:  cost, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is semidet.
%
% Backtracking solver incorporating a branch-and-bound strategy for optimisation problems.
% Here, the upper bound has to be provided.
% This allows to restrict the search to reasonable solutions, i.e., within a given "budget."
%
% Technically, the difficulty of branch-and-bound is to introduce some _sequentiality_.
% Schematically, the behaviour is as depicted below:
%    * Firstly, we must generate all the successor states of a given state (so-called "root" state).
%      Notice that we can restrict these successors to those with an edge cost strictly less than the current upper bound at the root state, i.e., UB.
%    * Secondly, we must scan all this successor states in order to find out which one, if it exists, contains the best solution.
%      This is accomplished thanks to a folding (a.k.a., "cumulative" _for-loop_ in imperative languages).
%      We start with no solution, only the known upper bound from the root state.
%      Then we scan all the successors, search for a possible better solution at each successor, and replace it, as well as the new upper bound, if such a _strictly_ better solution has been found.
%
% ==
%                                          +------------+
%                                          | root state |  Upper Bound (UB)
%                                          +------------+
%                                                 |
%                     +-----------------+-------------------+---------------------+
%                     | cost 1          | cost 2            | cost i              | cost n
%                     v                 v                   v                     v
%              +-------------+   +-------------+      +-------------+      +-------------+
%              | successor 1 |   | successor 2 | ...  | successor i | ...  | successor n |
%              +-------------+   +-------------+      +-------------+      +-------------+
%                     >                 >                   >                     >
%                     <                 <                   <                     <
%                     >                 >                   >                     >
%                     <                 <                   <                     <
% no_solution(UB) --> ? --------------> ! ------ ... -----> ! ------- ... ------> ? --> no_solution(UB) or solution(C_SS, SS)
% ==
%
% __Nota.__ Contrary to the other solvers, the branch-and-bound strategy is only semi-deterministic rather than non-deterministic.
%           In other words, it founds 0 or 1 solution rather than from 0 to an infinity.
%           This is due to the fact that whenever a solution has been found, we look for a _strictly_ better one and discard the previous best so far.
%           Should we accept equally costly solutions, we could face an _exponential_ number of sub-optimal solutions!
%           If one really wants to retrieve _all_ the best answers, this solver has first to be called in order to find out the optimal cost, then another solver could be run to retrieve all the solutions with exactly that best cost...
%
% @arg F    The final state predicate.
% @arg O    The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg S    The initial (or some "root") state.
% @arg UB   The _exclusive_ upper bound on the cost of a solution (to be set to "infinity", by 'solver/4', to be sure not to miss any solution).
% @arg SS   The path from the initial state to a final one, excluding the initial state.
%
% __Space complexity.__  O(b.d_max) where b is the average branching factor.
% The factor b is due to the fact that Prolog is not lazy, hence all the successors of a node have to be generated in order to execute the higher-order predicate 'fold/4'.
% In order to avoid this cost, it would be necessary to come back to a previous, much less readable implementation, where the fold was hand-coded.
%
% __Time complexity.__  O(b^d_max).
%
solver(F, _, S, _, []) :-
   call(F, S),  % i.e., final_state(S), at no cost
   !.
solver(F, O, S, UB, SS) :-
   findall( transition(D, NS, C)
          , ( call(O, D, S, NS, C)  % Firstly, search all possible transitions
            , C < UB                % Then, filter out those that are already too costly (should be useful only when being close to the leaves, but still worse using.)
            )
          , TS
          ),  % i.e., { transition(D, NS, C) : operation(D, S, NS, C), C < UB }.
   foldl(select_best(F, O), TS, no_solution(UB), solution(_, SS)).     % Finally, select only the very best solution, if it exists.

%! select_best(+F:  predicate(S:  state), +T:  transition(D:  description, NS:  state, C:  cost), +BS:  candidate_solution, -SS:  candidate_solution) is det.
%
% The "binary operator" that combines a successor output with the best solution so far.
%
% @arg F    The final state predicate.
% @arg O    The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg T    One of the transitions from the root state with D, its description, NS, the corresponding successor state, and C, the edge cost.
% @arg BS   The best candidate solution _so far_ that can be either no solution at all, just the strict root upper bound, or an actual solution along with its costs that is the de facto new upper bound.
% @arg SS   The best candidate solution chosen between the previous one and the one possibly obtained from this transition.
%
select_best(F, O, transition(D, NS, C), no_solution(UB), solution(C_SS, SS)) :-
   plus(UB_NS, C, UB),                   % (Guaranteed to be positive.)  
   solver(F, O, NS, UB_NS, SS_NS),       % Search for a solution starting at this successor.
   SS = [transition(D, NS, C) | SS_NS],  % If it succeeds, then we do have a new best solution for the "root" state.
   solution_cost(SS, C_SS),              % Its costs becomes the new strict upper bound.
   !.
select_best(F, O, transition(D, NS, C), solution(UB, _), solution(C_SS, SS)) :-
   C < UB,                               % Keep only a transition that has not become too costly in the meanwhile.
   plus(UB_NS, C, UB),                   % And apply the very same steps!
   solver(F, O, NS, UB_NS, SS_NS),
   SS = [transition(D, NS, C) | SS_NS],
   solution_cost(SS, C_SS),              % (Since the cost of this new solution is guaranteed to be strictly less than UB.)
   !.
select_best(_, _, _, BS, BS).            % Otherwise, transmit the previous best solution (or no solution) unchanged.

