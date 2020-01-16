% gvim:  fileencoding=utf8

/** <module> Crossing Frogs Problem Solved with a Simple Backtracking Inference Engine à la Prolog (Possibly Displaying the Reasoning Tree)

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
?- consult('frogs_solution.pl').
?- solve.
==
   
then ';' repeatedly to view all the solutions.

Try 'solve_and_animate' for puzzle-specific visualisations of the solutions.

Change the line of the comment place should you wish to see the reasoning tree.
Save, quit 'swipl' and restart.
Compare the output to the one of 'frogs_improved_solution.pl'.

 */

:- module(frogs_solution, [ solve/0
                          , solve_and_animate/0
                          ]).

:- use_module(frogs_problem, [ initial_state/1
                             , final_state/1
                             , operation/4
                             , frogs_pretty_solution/2
                             , frogs_pretty_animation/2
                             ]).

% Change the following module for the next one if you want to follow the reasoning tree at run time.
% Note that this requires to quit and reenter the SWI-Prolog shell in order to actually reload the code.
% (Otherwise an error occurs for trying to reload the already loaded 'solver/4' predicate.)
:- use_module(solver_naive, [ solver/4 ]).
% :- use_module(solver_naive_with_tracing, [ solver/4 ]).
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve is nondet.
%
% Solves the frogs puzzle then displays the solutions.
% As a side-effect, if the 'solver_naive_with_tracing' module as been loaded in place of 'solver_naive', shows all of the reasoning tree.
%
solve :-
   initial_state(I),
   solver(final_state, operation, I, S),
   frogs_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
   is_solution(frogs_problem:final_state, frogs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).

%! solve_and_animate is nondet.
%
% Solves the frogs puzzle then animates the solutions.
%
solve_and_animate :-
   initial_state(I),
   solver(final_state, operation, I, S),
   frogs_pretty_animation(I, S).

