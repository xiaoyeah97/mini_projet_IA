% gvim:  fileencoding=utf8

/** <module> Crossing Frogs Problem Solved with a Simple Backtracking Inference Engine à la Prolog

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * September 2014:  first version
   * January 2019:  modularisation;  documentation improvement
   * March 2019:  higher-order predicates;  separating the rendering of solutions

__USAGE__

Type in:

==
> swipl
?- consult('bridge_solution.pl').
?- solve.
==
   
then ';' repeatedly to view all the solutions.

Change the line of the comment place should you wish to see the reasoning tree.
Save, quit 'swipl' and restart.

 */

:- module(bridge_solution, [ solve/0 ]).

:- use_module(bridge_problem, [ initial_state/1
                              , final_state/1
                              , operation/4
                              , bridge_pretty_solution/2
                              ]).
:- use_module(solver_naive, [ solver/4 ]).
% :- use_module(solver_naive_with_tracing, [ solver/4 ]).
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve is nondet.
%
% Solves the bridge puzzle (with the naïve approach) then displays the solutions.
%
solve :-
   initial_state(I),
   solver(final_state, operation, I, S),
   bridge_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
   is_solution(bridge_problem:final_state, bridge_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).

