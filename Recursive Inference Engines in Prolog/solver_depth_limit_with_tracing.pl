% gvim:  fileencoding=utf8

/** <module> A Backtracking Inference Engine with Depth Limit and Tracing

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

:- module(solver_depth_limit_with_tracing, [ solver/4
                                           , solver/5
                                           ]).
:- meta_predicate solver(1, 4, ?, ?).
:- meta_predicate solver(1, 4, ?, ?, ?).
:- meta_predicate solver(1, 4, ?, ?, ?, ?, ?).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% A backtracking solver with (default) depth limit is a simple extension of the naïve version where the depth limit argument is added in order to avoid infinite searches into infinite state sub-graphs.
% It can be used both for _infinite_ state graphs and graphs with directed cycles.
% In other words, it can be used effectively to conduct searches on any kind of state graph.
% However, it is not expected to be efficient.
% In addition, should the depth limit be chosen too small, it will miss solutions.
%
% This is the public predicate.
% It simply calls its internal counterpart, 'solver/7', by setting the depth and trace limits to the maximal one given by 'solver_max_depth/1'.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   solver_max_depth(D_max),
   solver(F, O, I, SS, 0, D_max, D_max).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% A backtracking solver with (default) depth limit is a simple extension of the naïve version where the depth limit argument is added in order to avoid infinite searches into infinite state sub-graphs.
% It can be used both for _infinite_ state graphs and graphs with directed cycles.
% In other words, it can be used effectively to conduct searches on any kind of state graph.
% However, it is not expected to be efficient.
% In addition, should the depth limit be chosen too small, it will miss solutions.
%
% This is the public predicate.
% It simply calls its internal counterpart, 'solver/7', by setting the depth and trace limits to the maximal one given by 'solver_max_depth/1'.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   solver_max_depth(D_max),
   solver(F, O, I, SS, 0, D_max, D_max).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% Backtracking solver with depth limit and tracing.
%
% This is the public predicate.
% It simply calls its internal counterpart, 'solver/7', by setting the depth limit to the maximal one given by 'solver_max_depth/1'.
%
% @arg F       The final state predicate.
% @arg O       The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I       The initial state.
% @arg SS      The path from the initial state to a final one, excluding the initial state.
% @arg D_max   The maximal depth at which to conduct the search.
%
solver(F, O, I, SS, D_max) :-
   solver(F, O, I, SS, 0, D_max, D_max).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost)), +D_c:  natural, +D_max:  natural, +T_max:  natural) is nondet.
%
% Actual backtracking solver with given depth limit and tracing plus current depth in order to tabulate the traces.
%
% @arg F      The final state predicate.
% @arg O      The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg S      The initial state.
% @arg SS     The path from the initial state to a final one, excluding the initial state.
% @arg D_c    The current depth, also used as the indentation level for the running trace.
% @arg D_max  The maximal depth at which to conduct the search.
% @arg T_max  The maximal depth at which to trace the search.
%
solver(F, _, S, [], D_c, _, T_max) :-
   D_c =< T_max,  % to print only the first levels of the search tree
   call(F, S),    % i.e., final_state(S)
   writef("%rfinal_state: %t\n", [' | ', D_c, S]),
   !.
solver(F, _, S, [], D_c, _, T_max) :-
   D_c > T_max,
   call(F, S),
   !.
solver(F, O, S, [transition(D, NS, C) | SS], D_c, D_max, T_max) :-
   D_c =< D_max,
   D_c =< T_max,          % to print only the first levels of the search tree
   call(O, D, S, NS, C),  % i.e., operation(D, S, NS, C)
   writef("%rTry %t: %t\n", [' | ', D_c, O, S]),
   plus(D_c, 1, D_c_plus_1),
   solver(F, O, NS, SS, D_c_plus_1, D_max, T_max).
solver(F, O, S, [transition(D, NS, C) | SS], D_c, D_max, T_max) :-
   D_c =< D_max,
   D_c > T_max,
   call(O, D, S, NS, C),  % i.e., operation(D, S, NS, C)
   plus(D_c, 1, D_c_plus_1),
   solver(F, O, NS, SS, D_c_plus_1, D_max, T_max).
solver(_, _, _, _, D_c, _, T_max) :-
   D_c =< T_max,
   writef("%rNo (more) operation is possible\n", [' | ', D_c]),
   fail.

%! solver_max_depth(-D_max:  natural) is det.
%
% @arg D_max   The default maximal depth at which the search is conducted by this solver.
%
solver_max_depth(20).

