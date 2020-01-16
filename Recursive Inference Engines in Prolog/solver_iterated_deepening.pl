% gvim:  fileencoding=utf8

/** <module> A Backtracking Inference Engine with Iterated Deepening Search

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

:- module(solver_iterated_deepening, [ solver/4
                                     , solver/5
                                     ]).
:- meta_predicate solver(1, 4, ?, ?).
:- meta_predicate solver(1, 4, ?, ?, ?).

:- use_module(solver_depth_limit, [ solver/5 as solver_depth_limited ]).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +I:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost))) is nondet.
%
% This should be a meta-algorithm.
% Here, it relies directly on the depth-limited search algorithm.
% It could have derived on any kind of algorithm accepting a depth-limit.
%
% This iterated-deepening (meta) solver (with default maximal depth limit) tries to circumvent the main limitation of the depth-limit solver.
% In the latter, the depth-limit is chosen _in advance_, which can turn out to be difficult:
% Should it be _too small_, we shall miss solutions;
% Should it be _too large_, we shall waste a lot of time in large and unproductive branches.
% Here, we start with a minimal depth-limit of one an increment it whenever no solution has been found.
% In order to avoid supposedly infinite searches (or a least too expensive ones), a maximal depth can be given.
% A rather large one is provided by default.
%
% This solver is both effective and rather efficient.
%
% Basically, it repeats searches at deeper and deeper levels in order to find a solution.
% Therefore, it is a kind of breadth-first search.
% Hence, it is optimal with respect to the length of the solution (not necessarily its cost).
% Also, it is complete as long as the state graph is locally finite (i.e., no operation generates an infinity of successor states).
%
% Finally, its asymptotic time complexity is the same as the underlying depth-limited search.
% Effectively, running iteratively an exponential algorithm at most doubles its time complexity, which is a very small multiplicative constant!
%
% @arg F   The final state predicate.
% @arg O   The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I   The initial state.
% @arg SS  The path from the initial state to a final one, excluding the initial state.
%
solver(F, O, I, SS) :-
   iterated_deepening_max_depth(D_max),
   solver(F, O, I, SS, D_max).

%! solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost)), +D_max:  int) is nondet.
%
% Iterated-deepening solver with given maximal depth limit.
%
% This is the public predicate.
% It simply calls its internal counterpart, 'solver/6', by setting the current depth limit to 1.
%
% @arg F      The final state predicate.
% @arg O      The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I      The initial state.
% @arg SS     The path from the initial state to a final one, excluding the initial state.
% @arg D_max  The maximal depth at which the search can be conducted from the initial state.
%
solver(F, O, I, SS, D_max) :-
   meta_solver(F, O, I, SS, 1, D_max).

%! meta_solver(+F:  predicate(S:  state), +O:  predicate(D:  description, S:  state, NS:  state, C:  cost), +S:  state, -SS:  list(transition(D:  description, NS:  state, C:  cost)), +D_c:  int, +D_max:  int) is nondet.
%
% This is actually the meta algorithm of this module.
% It iterates depth-limited searches at increasingly deeper levels, up to a maximal depth.
%
% __Nota.__ When providing a nondet predicate, the solutions under the maximal depth limit will be found several times.
%
% @arg F      The final state predicate.
% @arg O      The operation predicate where D is a description of the applied operation (basically its name), S is the original state, NS is the resulting state, and C is the cost of this transition.
% @arg I      The initial state.
% @arg SS     The solution path from the initial state to a final one, excluding the initial state.
% @arg D_c    The current depth at which the actual search is to be conducted by the underlying depth-limited search algorithm.
% @arg D_max  The maximal depth at which the search can be conducted from the initial state.
%
meta_solver(F, O, I, SS, D_c, D_max) :-
   D_c =< D_max,                            % If the maximal depth has not yet been reached (without finding any solution)...
   solver_depth_limited(F, O, I, SS, D_c).  % ... then start a new search at the current depth limit...
   %  !.                                    % ... and add a cut to stop searching at the optimal depth and the very first solution if successful.
meta_solver(F, O, I, SS, D_c, D_max) :-
   D_c =< D_max,                            % After searching the solutions at a given depth, ...
   plus(D_c, 1, D_c_plus_1),                % ... we start again, searching at a deeper level.
   meta_solver(F, O, I, SS, D_c_plus_1, D_max).
% meta_solver(_, _, _, _, D_c, D_max) :-
%    D_c > D_max,                           % Otherwise, the maximal depth has been reached (without finding any solution)...
%    fail.                                  % ... then we stop searching in order to avoid too lengthy searches as well as infinite ones!

%! iterated_deepening_max_depth(?D_max:  int) is det.
%
% @arg D_max   The default maximal depth at which the searches are conducted by this meta-solver.
%
iterated_deepening_max_depth(20).

