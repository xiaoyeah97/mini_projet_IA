% gvim:  fileencoding=utf8

/** <module> Filling Jugs Problem Solved with a Backtracking Inference Engine with Iterated Deepening (and Possibly Reasoning Tree Tracing)

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * 2014:  first version
   * January 2019:  modularisation;  documentation improvement
   * March 2019:  higher-order predicates

__USAGE__

Type in:

==
> swipl
?- consult('jugs_solution_iterated_deepening.pl').
?- solve(6).
==

Try

==
?- solve(2).
==

then

==
?- solve(10).
==

Then ';' repeatedly to view all the solutions in increasing order of complexity.
   
What do you observe?  How can you deal with duplicates?
   
 */

:- module(jugs_solution_iterated_deepening, [ solve/1 ]).

:- use_module(jugs_problem, [ initial_state/1
                            , final_state/1
                            , operation/4
                            , jugs_pretty_solution/2
                            ]).
:- use_module(solver_iterated_deepening, [ solver/5 ]).
% :- use_module(solver_iterated_deepening_with_tracing, [ solver/5 ]).
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve(+D_max:  int) is nondet.
%
% Solves the problem, within a maximal depth, and displays the solutions step-by-step in a specific way.
% In case the module 'solver_iterated_deepening_with_tracing' replaces 'solver_iterated_deepening', also shows the successive restarts at deeper and deeper levels of search along with the reasoning tree at each level.
%
% @arg D_max  The maximal depth at which the search in to be conducted.
%
solve(D_max) :-
   initial_state(I),
   solver(final_state, operation, I, S, D_max),
   jugs_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
   is_solution(jugs_problem:final_state, jugs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).

