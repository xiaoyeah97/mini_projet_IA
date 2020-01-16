% gvim:  fileencoding=utf8

/** <module> A Backtracking Inference Engine with Informed Branch-and-Bound and Directed-cycle Detection

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * 2014:  first version
   * January 2019:  modularisation;  documentation improvement
   * March 2019:  higher-order predicates;  especially folding in order to clarify the rather complex processing logic of the branch-and-bound strategy;  combining with directed-cycle detection

__USAGE__
  
To be imported by a problem (solution) module.

 */

:- module(solver_informed_branch_and_bound_with_tracing, [ solver/4
                                                         , solver/5
                                                         , solver/6
                                                         , no_evaluation_heuristic/2
                                                         ]).
:- meta_predicate solver(1, 4, ?, ?).
:- meta_predicate solver(1, 4, 2, ?, ?).
:- meta_predicate solver(1, 4, 2, ?, ?, ?).
:- meta_predicate solver(1, 4, 2, ?, ?, ?, ?, ?).

:- use_module(solver_common, [ solution_cost/2 ]).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is semidet.
%
% Backtracking solver incorporating a branch-and-bound strategy for optimisation problems, more specifically _minimisation_ ones.
%
% __Nota.__ A maximisation problem can be translated into a minimisation one by negating the scores.
%
% This is the simplest version.
% It simply calls its explicit counterpart, 'solver/5', by initialising the heuristic evaluation function to 'no_evaluation_function/2'!
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The solution as a path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   solver(F, O, no_evaluation_heuristic, I, SS).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +H:  predicate(S:  state, HC:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is semidet.
%
% This is the second most general version, an actually informed version.
% This version allows to provide a heuristic predicate in order to solve a problem instance faster.
% It calls its explicit counterpart, 'solver/6', by initialising the upper bound to infinity.
% This default behaviour can turn the search quite slow should the heuristic be insufficient at the highest levels of the tree, especially the root node.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg H   The heuristic cost evaluation predicate where S is the state and HC its heuristic cost _by default_ to reach a possible solution.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, H, I, SS) :-
   solver(F, O, H, I, +1Inf, SS).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +H:  predicate(S:  state, HC:  cost), +I:  state, +UB:  integer, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is semidet.
%
% Besides the heuristic, this version of the solver allows to provide a problem-specific upper bound.
%
% This is certainly the most common entry point.
% It simply calls its explicit counterpart, 'solver/8', by initialising inner parameters, namely the (empty) list of ancestors for directed-cycle detection and the (null) indentation level for tracing.
%
% __Nota.__ The implementation relies only on integer-based scores.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg H   The heuristic cost evaluation predicate where S is the state and HC its heuristic cost _by default_ to reach a possible solution.
% @arg I   The initial state.
% @arg UB  A (strict) upper bound on the cost of a solution.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, H, I, UB, SS) :-
   solver(F, O, H, I, UB, SS, [], 0).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +H:  predicate(S:  state, HC:  cost), +S:  state, +UB:  cost, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is semidet.
%
% Backtracking solver incorporating a branch-and-bound strategy for optimisation problems.
% Here, the upper bound has to be provided.
% This allows to restrict the search to reasonable solutions, i.e., within a given "budget."
%
% Technically, the difference with the _uninformed_ version of branch-and-bound is the use of the heuristic to locally sort the branches in order to improve the likelihood to find the best solution in the first branch(es).
%
% @arg F    The final state predicate.
% @arg O    The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg H    The heuristic cost evaluation predicate where S is the state and HC its heuristic cost _by default_ to reach a possible solution.
% @arg S    The initial (or some "root") state.
% @arg UB   The _exclusive_ upper bound on the cost of a solution (to be set to "infinity", by 'solver/4', to be sure not to miss any solution).
% @arg SS   The path from the initial state to a final one, excluding the initial state.
% @arg AS   The ancestor states.
% @arg T    The depth of the search, to be used for tracing purposes.
%
% __Space complexity.__  O(b.d_max) where b is the average branching factor.
% Contrary to the _uniformed_ version of this algorithm, the factor b cannot be avoided since we have to sort the successors in increasing heuristic cost.
%
% __Time complexity.__  O(b^d_max).
%
solver(F, _, _, S, _, [], _, T) :-  % There is no transition in the solution...
   call(F, S),                      % ... should the given state already be a final one (i.e., final_state(S)), at no cost
   writef("%r%t is a final state\n", [' | ', T, S]),
   !.                               % And certainly do _not_ try to find redundant solutions as extensions of this one!
solver(F, O, H, S, UB, SS, AS, T) :-
   findall( transition(D, NS, C, HC)
          , ( call(O, D, S, NS, C)     % Firstly, search all the possible transitions, i.e., successors.
            , C < UB                   % Then, filter out those that are already too costly (should be useful only when being close to the leaves, but still worse using.)
            , \+ member(NS, [S | AS])  % Also, remove those that belong to ancestors, i.e., avoid infinite directed cycles!
            , call(H, NS, HC_NS)       % Next, apply the heuristic _by default_ evaluation from the successor to a possible solution.
            , plus(C, HC_NS, HC)       % The complete heuristic cost from the root state is to be used by the following sort.
            , HC < UB                  % Further filter out on the heuristic cost.
            )
          , TS
          ),
   sort(4, @=<, TS, TS_sorted),     % Next, the successors are sorted in increasing heuristic cost, i.e., the 4th component.
   length(TS, N_TS),
   writef("%r%t has %t competitive (non-ancestor) successors (upper bound = %t)\n", [' | ', T, S, N_TS, UB]),
   plus(T, 1, T_plus_1),
   foldl(select_best(F, O, H, [S | AS], T_plus_1),
         TS_sorted, 
         no_solution(UB), 
         solution(_, SS)).          % Finally, select only the very best solution, if it exists.
solver(_, _, _, _, _, _, _, T) :-
   writef("%rNo (more) operation is possible\n", [' | ', T]),
   fail.

%! select_best(+F:  predicate(S:  state), +O:  transition(D:  description, NS:  state, C:  cost, HC:  cost), +BS:  candidate_solution, -SS:  candidate_solution) is det.
%
% The "binary operator" that combines a successor output with the best solution so far.
%
% @arg F    The final state predicate.
% @arg O    The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg H    The heuristic cost evaluation predicate where S is the state and HC its heuristic cost _by default_ to reach a possible solution.
% @arg AS   The ancestor states.
% @arg T    One of the transitions from the root state with D, its description, NS, the corresponding successor state, C, the edge cost, and HC, the heuristic cost of the best solution in this branch.
% @arg BS   The best candidate solution _so far_ that can be either no solution at all, just the strict root upper bound, or an actual solution along with its costs that is the de facto new upper bound.
% @arg SS   The best candidate solution chosen between the previous one and the one possibly obtained from this transition.
%
select_best(F, O, H, AS, T, 
            transition(D, NS, C, _), 
            no_solution(UB), 
            solution(C_SS, SS)) :-
   writef("%rNo solution yet.  What about %t to %t?\n", [' | ', T, D, NS]),
   plus(UB_NS, C, UB),                       % (Guaranteed to be positive.)  
   solver(F, O, H, NS, UB_NS, SS_NS, AS, T), % Search for a solution starting at this successor.
   SS = [transition(D, NS, C) | SS_NS],      % If it succeeds, then we do have a new best solution for the "root" state.
   solution_cost(SS, C_SS),                  % Its cost becomes the new strict upper bound.  (Must be less or equal to the heuristic cost.)
   writef("%rFirst solution has been found:  %t, with cost %t\n", [' | ', T, SS, C_SS]),
   !.
select_best(F, O, H, AS, T,
            transition(D, NS, C, _),
            solution(UB, _),
            solution(C_SS, SS)) :-
   writef("%rAlready a solution.  But could %t to %t be better?\n", [' | ', T, D, NS]),
   C < UB,                                   % Keep only a transition that has not become too costly in the meanwhile.
   plus(UB_NS, C, UB),                       % And apply the very same steps!
   solver(F, O, H, NS, UB_NS, SS_NS, AS, T),
   SS = [transition(D, NS, C) | SS_NS],
   solution_cost(SS, C_SS),                  % (Since the cost of this new solution is guaranteed to be strictly less than UB.)
   writef("%rNew better solution has been found:  %t, with cost %t\n", [' | ', T, SS, C_SS]),
   !.
select_best(_, _, _, _, T,
            _,
            BS,
            BS) :-                           % Otherwise, transmit the previous best solution (or no solution) unchanged.
   writef("%rNo better solution found so far\n", [' | ', T]).

%! no_evaluation_heuristic(+S:  state, HC:  cost) is det.
%
% This predicate provides the uninformed heuristic!
% However, there is a difference between the uninformed branch-and-bound algorithm and this branch-and-bound version with an explicit uninformed heuristic!
% In the former one, the branches remain unsorted.
% In this case, the branches are sorted though only on the known cost of the transitions.
%
% @arg S    Some state.
% @arg HC   An always satisfying default heuristic evaluation:  zero!
%
no_evaluation_heuristic(_, 0).

