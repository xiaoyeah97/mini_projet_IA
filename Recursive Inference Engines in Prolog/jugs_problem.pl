% gvim:  fileencoding=utf8

/** <module> The Jugs Problem

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__HISTORY__

   * 2014:  first version
   * January 2019:  modularisation;  documentation improvement
   * March 2019:  merging of a former pretty-printing module with this one

__CONTENTS__

Following the recommended methodology, the description of the solution to a problem exports exactly three predicates:
   * 'initial_state/1', that provides the description of an (example) initial problem (possibly several);
   * 'final_state/1', that verifies if a given state is a solution to the problem;
   * 'operation/4', that applies an authorised transition to a given state, giving a new state for a given cost of the transition.

__RIDDLE__

You got three jugs, the capacities of which are respectively 3, 5, and 11 liters.
You need to obtain exactly 2 liters.
You have no other way to evaluate the quantity of water than the three jugs themselves (i.e., their capacities).
You have a river at your disposal to fill them right to the top.
Would you be able to measure those 2 liters?

See [[on YouTube][http://www.youtube.com/watch?v=BVtQNK_ZUJg]] for a similar problem.

 */

:- module(jugs_problem, [ initial_state/1
                        , final_state/1
                        , operation/4
                        , jugs_pretty_solution/2
                        ]).

%! initial_state(-S:  list(jug(Q:  int, C:  int))) is det.
%
% The single instance of problem to solve as a list of jugs with the initial contents.
%
% Each jug is represented by a couple consisting of its quantity of water and its full capacity, both as positive integers.
% Obviously, the quantity has to be less than or equal to the capacity.
%
% @arg S  A state, as a list of jug contents consisting of Q, a quantity, and C, a capacity.
%
initial_state([ jug(0,  3)
              , jug(0,  5)
              ]).

:- begin_tests(jugs_initial_state).

test(list_of_jugs) :-
   initial_state(JS),
   assertion( forall( member(J, JS)
                    , J = jug(_, _)
                    ) ).

test(positive_quantities) :-
   initial_state(JS),
   assertion( forall( member(jug(Q, _), JS)
                    , Q >= 0
                    ) ).

test(quantities_less_than_or_equal_capacities) :-
   initial_state(JS),
   assertion( forall( member(jug(Q, C), JS)
                    , Q =< C
                    ) ).

:- end_tests(jugs_initial_state).

%! final_state(?S:  list(jug(Q:  int, C:  int))) is det.
%
% The final states.
% Here, at least one jug must contain two liters in it.
% This predicate can be modified at will in order to find any kind of solution, e.g., two jugs with the same quantity, etc.
%
% @arg S   A state. 
%
final_state(S) :-
   member(jug(4, _), S).

%! operation(-D:  description, +S:  state, -NS:  state, -T:  cost) is det.
%
% Three operations can be applied to a state:
%    * filling a jug that is not yet full;
%    * emptying a jug that is not yet empty;
%    * transferring water from a non-empty jug to an non-full one.
%
% The cost of an operation is the number of liters that are manipulated.
%
% @arg D   The description of the applied operation, possibly with it "curried" parameters.
% @arg S   The current state.
% @arg NS  The new state obtained by applying the operation to the current state.
% @arg T   The cost of the operation in the number of transferred liters.
%
% \bug  Several things can go wrong with one or another solver depending on the way the following rules are written.
% For instance, one could move the "move" operation before "fill" and "empty"...
% Also, one can decide than moving water between jugs costs nothing rather than "T" since no new water has been used...
%
operation(fill(J), S, NS, T) :-
   append(L, [J | R], S),
   J = jug(Q, C),
   Q < C,                           % if th pre-condition is OK...
   append(L, [jug(C, C) | R], NS),  % ... then apply...
   T is C - Q.                      % ... and determine cost.
operation(empty(J), S, NS, Q) :-
   append(L, [J | R], S),
   J = jug(Q, C),
   Q > 0,                           % pre-condition
   append(L, [jug(0, C) | R], NS).  % apply
operation(move(J1, J2), S, NS, T) :-
   append(L, [J1 | MR], S),
   append(M, [J2 | R ], MR),
   J1 = jug(Q1, C1),
   J2 = jug(Q2, C2),
   Q1 > 0,                      % pre-condition
   Q2 < C2,
   T      is min(Q1, C2 - Q2),  % apply
   Q1_new is Q1 - T,
   Q2_new is Q2 + T,
   append(L,  [jug(Q1_new, C1) | M], LM),
   append(LM, [jug(Q2_new, C2) | R], NS).
operation(move(J2, J1), S, NS, T) :-  % move the other way round
   append(L, [J2 | MR], S),
   append(M, [J1 | R ], MR),
   J2 = jug(Q2, C2),
   J1 = jug(Q1, C1),
   Q1 > 0,
   Q2 < C2,
   T      is min(Q1, C2 - Q2),
   Q1_new is Q1 - T,
   Q2_new is Q2 + T,
   append(L,  [jug(Q2_new, C2) | M], LM),
   append(LM, [jug(Q1_new, C1) | R], NS).

%! jugs_pretty_solution(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is det.
%
% Solution beautifier:  displays the solutions step-by-step in a more compact version than the general beautifier.
%
% @arg I    The initial state of the following solution.
% @arg SS   The solution as a sequence of transition from I to some final state.
%
jugs_pretty_solution(I, SS) :-
   nl,
   jugs_pretty_state(I),                    % displays the initial jugs contents...
   nl,
   forall( member(transition(_, S, _), SS)  % ... then the sequence of jug contents along a solution path.
         , ( jugs_pretty_state(S)
           , nl
           )
         ),
   nl.

%! jugs_pretty_state(+S:  State) is det.
%
% Solution beautifier:  displays a single state.
%
% @arg S   A state.
%
jugs_pretty_state(S) :-
   forall( member(J, S)
         , pretty_jug(J)
         ).

%! pretty_jug(+J:  jug(Q:  int, C:  int)) is det.
%
% Displays the contents of a single jug.
%
% @arg J   A jug where Q is its quantity and C its capacity.
%
pretty_jug(jug(0, C)) :-
   writef("_|%t   ", [C]).
pretty_jug(jug(C, C)) :-
   writef("▮|%t   ", [C]).
pretty_jug(jug(Q, C)) :-
   0 < Q, Q < C,
   writef("%t|%t   ", [Q, C]).

:- run_tests.

