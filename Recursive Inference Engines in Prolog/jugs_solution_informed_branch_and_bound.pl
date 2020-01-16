% gvim:  fileencoding=utf8

/** <module> Filling Jugs Problem Solved with a Backtracking Inference Engine with (Hardly Informed) Branch-and-Bound (and Possibly Reasoning Tree Tracing)

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
?- consult('jugs_solution_informed_branch_and_bound.pl').
?- solve.
==

 */

:- module(jugs_solution_informed_branch_and_bound, [ solve/0 ]).

:- use_module(jugs_problem, [ initial_state/1
                            , final_state/1
                            , operation/4
                            , jugs_pretty_solution/2
                            ]).
:- use_module(solver_informed_branch_and_bound, [ solver/5
                                                , solver/6
                                                , no_evaluation_heuristic/2
                                                ]).
/*
:- use_module(solver_informed_branch_and_bound_with_tracing, [ solver/5
                                                             , solver/6
                                                             , no_evaluation_heuristic/2
                                                             ]).
*/
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve is nondet.
%
% Solves the problem by showing all of the reasoning tree.
%
solve :-
   initial_state(I),
   writef("Initial state:  %t\n", [I]),
   jugs_max_bound(B),
   writef("Max bound:  %t\n", [B]),
   solver(final_state, operation, no_evaluation_heuristic, I, B, S),
   writef("Solution:  %t\n", [S]),
   jugs_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
   is_solution(jugs_problem:final_state, jugs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).

%! jugs_max_bound(?B:  int) is det.
%
% Some maximal liters of water to be used in order to solve the jug problem.
%
jugs_max_bound(1000).

