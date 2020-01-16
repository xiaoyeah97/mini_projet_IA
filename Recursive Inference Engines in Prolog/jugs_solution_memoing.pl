% gvim:  fileencoding=utf8

/** <module> Filling Jugs Problem Solved with a Backtracking Inference Engine with Memoing

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * 2014:  first version
   * June 2019:  modularisation

__USAGE__

Type in:

==
> swipl
?- consult('jugs_solution_memoing.pl').
?- solve.
==

then ';' to see the various solutions.

Stop resolution at various number of solutions and look at the memoing memory ('memoing(X, Y)').  
What do you observe?

Check that you have a mapping indeed with 'findall(X, memoing(X, _), LX), length(LX, NL), list_to_set(LX, SX), length(SX, NS).'.
Run to the end the solver.
Can you retrieve the whole search graph in 'memoing'?
Yes?  How?
   
 */

:- module(jugs_solution_memoing, [ solve/0 ]).

:- use_module(jugs_problem, [ initial_state/1
                            , final_state/1
                            , operation/4
                            , jugs_pretty_solution/2
                            ]).
:- use_module(solver_memoing, [ solver/4 ]).
% :- use_module(solver_memoing_with_tracing, [ solver/4 ]).
:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).

%! solve is nondet.
%
% Solves the problem and displays the solutions step-by-step in a specific way.
%
solve :-
   initial_state(I),
   solver(final_state, operation, I, S),
   jugs_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
   is_solution(jugs_problem:final_state, jugs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).



























































































































































































































/* ANSWER TO THE LAST QUESTION

Not totally since we keep only the shortest paths.

'findall((X, Y), memoing(X, [step(_, Y)|_]), G), forall(member((X, Y), G), (write((X, Y)), nl)).' extract some edges.

*/




































































































































































































