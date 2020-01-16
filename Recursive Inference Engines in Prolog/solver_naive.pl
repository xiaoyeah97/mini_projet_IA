% gvim:  fileencoding=utf8

/** <module> A Simple Backtracking Inference Engine à la Prolog

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
  
To be imported by a problem (solution) module.

 */

:- module(solver_naive, [ solver/4 ]).
:- meta_predicate solver(1, 4, ?, ?).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% This is the _very basic_ backtracking solver...
%
% It can be safely used only for _finite_ and _directed acyclic_ state graphs.
% This engine closely mimics the way Prolog solves a problem.
% Hence it is generally not to be used!
% In particular, it founds _all_ the solutions, including the ones that are extensions of a previous one.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg S   The initial (or some) state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
% @bug  Sorry, the backtracking solver code is really that short and easy!
%
solver(F, _, S, []) :-   % There is no transition in the solution...
   call(F, S).           % ... should the given state already be a final one (i.e., final_state(S)).
solver(F, O, S, [transition(D, NS, C) | SS]) :-  % The solution is the concatenation of...
   call(O, D, S, NS, C),                         % ... a transition from the current state (i.e., operation(D, S, NS, C)) and...
   solver(F, O, NS, SS).                         % ... the solution from the resulting state

