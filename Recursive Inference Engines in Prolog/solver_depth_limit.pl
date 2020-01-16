% gvim:  fileencoding=utf8

/** <module> A Backtracking Inference Engine with Depth Limit

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

:- module(solver_depth_limit, [ solver/4
                              , solver/5
                              ]).
:- meta_predicate solver(1, 4, ?, ?).
:- meta_predicate solver(1, 4, ?, ?, ?).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% A backtracking solver with (default) depth limit is another simple extension of the naïve version.
% A depth limit argument is added in order to avoid infinite searches into infinite state sub-graphs.
% It can be used both for _infinite_ state graphs and graphs with directed cycles.
% In other words, it can be used effectively to conduct searches on any kind of state graph.
% However, it is not expected to be efficient.
% In addition, should the depth limit be chosen too small, it will miss solutions.
%
% This is the public predicate.
% It simply calls its internal counterpart, 'solver/5', by setting the depth limit to the maximal one given by 'solver_max_depth/1'.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   solver_max_depth(D_max),
   solver(F, O, I, SS, D_max).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost)), +D_max:  int) is nondet.
%
% Backtracking solver with given depth limit.
%
% @arg F      The final state predicate.
% @arg O      The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg S      The initial state.
% @arg SS     The path from the initial state to a final one, excluding the initial state.
% @arg D_max  The maximal depth at which the search can be conducted from the initial state.
%
solver(F, _, S, [], _) :-
   call(F, S),  % i.e., final_state(S)
   !.
solver(F, O, S, [transition(D, NS, C) | SS], D_max) :-
   D_max > 0,
   call(O, D, S, NS, C),  % i.e., operation(D, S, NS, C)
   plus(D_max_minus_1, 1, D_max),
   solver(F, O, NS, SS, D_max_minus_1).

%! solver_max_depth(-D_max:  int) is det.
%
% @arg D_max   The default maximal depth at which the search is conducted by this solver.
%
solver_max_depth(20).

