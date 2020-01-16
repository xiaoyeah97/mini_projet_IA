% gvim:  fileencoding=utf8

/** <module> Several Utility Predicates Related to a Problem and one of its Solutions

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * 2014:  first version
   * January 2019:  modularisation;  documentation improvement;  lengthy but visual enhanced display
   * March 2019:  adding some predicates and changing the name of this module

__USAGE__
  
To be imported by a problem (solution) module.

 */

:- module(solver_common, [ is_solution/4
                         , solution_cost/2
                         , pretty_solution/2
                         ]).

%! is_solution(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state,  +SS:  list(transition(D:  description, NS:  state, C:  cost))) is det.
%
% Predicate that verifies that a claimed solution to a problem is indeed a solution.
%
% @arg F    The final state predicate.
% @arg O    The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg S    The "initial" state with respect to the given solution.
% @arg SS   A supposed solution as the sequence of operations to apply from the given initial state in order to reach a final state.
%
is_solution(F, _, S, []) :-
   call(F, S).  % i.e., final_state(S)
is_solution(F, O, S, [transition(D, NS, C) | SS]) :-
   call(O, D, S, NS, C),  % i.e., operation(D, S, NS, C)
   is_solution(F, O, NS, SS).

%! solution_cost(+SS:  list(transition(D:  description, NS:  state, C:  cost)), -C:  cost) is nondet.
%
% Computes the cost of a solution as the sum of the costs of its transitions.
%
% __Nota.__ From the complexity point of view, this predicate can generally be safely used during searches.
%           Searching a tree is normally an exponential process.
%           (Re)Computing the cost of a solution is only linear.
%
% @arg P   The path from some (the initial) state to another (a final) one, excluding the initial state.
% @arg C   The cost of the path as the sum of the costs associated to all the transitions on this path.
%
solution_cost([], 0).
solution_cost([transition(_, _, C_T) | P], C) :-
   solution_cost(P, C_P),
   plus(C_P, C_T, C).

%! pretty_solution(+I:  state,  +SS:  list(transition(D:  description, NS:  state, C:  cost))) is det.
%
% Solution beautifier:  displays a solution step-by-step in a standardised way.
%
% In order to clearly separate the components of the transitions, each appear on different lines on the console.
% Consecutive states are connected by a vertical arrow.
% Arrows are labelled by the operation and its corresponding cost.
%
% @arg I    The initial state of a problem to solve.
% @arg SS   The found solution as the sequence of operations to apply from the initial state in order to reach a final state.
%
pretty_solution(I, SS) :-
   writef("Problem is:\n\n%t\n\n", [I]),
   length(SS, N_SS),
   writef("Found a solution in %t steps\n", [N_SS]),
   forall( member(transition(D, S, C), SS)
         , writef("   |\n   | %t\n   | $ %t\n   |\n   v\n%t\n", [D, C, S])
         ),
   nl,
   nl.

