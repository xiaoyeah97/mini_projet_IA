% gvim:  fileencoding=utf8

/** <module> A Backtracking Inference Engine with Directed Cycle Detection and Tracing

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

:- module(solver_directed_cycle_detection_with_tracing, [ solver/4 ]).
:- meta_predicate solver(1, 4, ?, ?).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% This backtracking solver with directed cycle detection slightly extends the naïve version.
% Its goal is to avoid one of its two main problems, namely searching round and round into infinite directed cycles!
% This is achieved by keeping a record of the path from the initial state to the current one.
% Notice that this extension is sufficient for _finite_ state graphs.
% However, it remains inefficient when the graph contains a lot of _undirected_ cycles, but this remains a big problem for any kind of solver...
%
% This is the public predicate.
% It simply calls its internal counterpart, 'solver/6', with no ancestors, and a current depth of zero.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   solver(F, O, I, SS, [], 0).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost)), +AS:  list(state), +D:  int) is nondet.
%
% Actual backtracking solver with directed cycle detection.
% As a side-effect, it displays its reasoning tree.
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg S   The initial (or some) state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
% @arg AS  The ancestor states.
% @arg T   The indentation level (a.k.a., depth) for visualising the trace as a tree.
%
solver(F, _, S, [], _, T) :-
   call(F, S),  % i.e., final_state(S)
   writef("%rFinal state %t\n", [' | ', T, S]),
   !.
solver(_, _, S, _, AS, T) :-
   member(S, AS),
   writef("%rBacktracking on already found state %t\n", [' | ', T, S]),
   fail.
solver(F, O, S, [transition(D, NS, C) | SS], AS, T) :-
   \+ member(S, AS),
   call(O, D, S, NS, C),  % i.e., operation(D, S, NS, C)
   writef("%rTry %t on state %t gives %t\n", [' | ', T, O, S, NS]),
   plus(T, 1, T_plus_1),
   solver(F, O, NS, SS, [S | AS], T_plus_1).
solver(_, _, _, _, _, T) :-
   writef("%rNo (more) operation is possible\n", [' | ', T]),
   fail.

