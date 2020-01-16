% gvim:  fileencoding=utf8

/** <module> Filling Jugs Problem Solved with a Backtracking Inference Engine with Depth Limit

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
?- consult('jugs_solution_depth_limit.pl').

==

Try 

==
?- solve(12).
==

then ';' repeatedly to view *all* the solutions!

Then, try

==
?- solve(8).
==

next

==
?- solve(6).
==

and finally

==
?- solve(4).
==

What do you observe?

 */

:- module(jugs_solution_depth_limit, [ solve/1 ]).

:- use_module(jugs_problem, [ initial_state/1
                            , final_state/1
                            , operation/4
                            , jugs_pretty_solution/2
                            ]).
:- use_module(solver_depth_limit, [ solver/5 ]).
% :- use_module(solver_depth_limit_with_tracing, [ solver/5 ]).
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve(+D_max:  int) is nondet.
%
% Solves the problem, within a maximal depth, and displays the solutions step-by-step in a specific way.
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

