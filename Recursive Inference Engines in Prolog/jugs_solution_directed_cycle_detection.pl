% gvim:  fileencoding=utf8

/** <module> Filling Jugs Problem Solved with a Backtracking Inference Engine with Directed Cycle Detection (and Possibly Reasoning Tree Tracing)

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
?- consult('jugs_solution_directed_cycle_detection.pl').
?- solve.
==

then ';' to see the various solutions.

 */

:- module(jugs_solution_directed_cycle_detection, [ solve/0 ]).

:- use_module(jugs_problem, [ initial_state/1
                            , final_state/1
                            , operation/4
                            , jugs_pretty_solution/2
                            ]).
:- use_module(solver_directed_cycle_detection, [ solver/4 ]).
% :- use_module(solver_directed_cycle_detection_with_tracing, [ solver/4 ]).
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve is nondet.
%
% Solves the problem and displays the solutions step-by-step in a specific way.
% Also, should the module 'solver_directed_cycle_detection_with_tracing' be used in place of 'solver_directed_cycle', displays the whole reasoning tree as a side-effect.
%
solve :-
   initial_state(I),
   solver(final_state, operation, I, S),
   jugs_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
   is_solution(jugs_problem:final_state, jugs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).

