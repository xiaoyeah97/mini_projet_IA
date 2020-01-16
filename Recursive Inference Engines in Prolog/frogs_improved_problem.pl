% gvim:  fileencoding=utf8

/** <module> The Crossing Frogs Problem
 *
 * @author José Martinez
 * @see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
 * @license All Rights Reserved
 *
 * This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
 * It is provided only for a pedagogical usage at Polytech Nantes.
 *
 * @version 1.0
 *
 * __HISTORY__
 *
 *    * March 2019:  first version
 *
 * __CONTENTS__
 *
 * Following the recommended methodology, the description of the solution to a problem exports exactly three predicates:
 *    * 'initial_state/1', that provides the description of an (example) initial problem (possibly several);
 *    * 'final_state/1', that verifies if a given state is a solution to the problem;
 *    * 'operation/4', that applies an authorised transition to a given state, giving a new state for a given cost of the transition.
 *
 * __RIDDLE__
 *
 * Some red frogs arrive from the west, whereas green frogs travel from the east.
 * Their parties have to cross a river jumping from water lily to water lily.
 * They found themselves in the middle of the river.
 * They can jump on the next water lily if it is empty.
 * They can also jump over a single other frog, again if the following water lily is empty.
 * Help them to cross each other and continue on their travels!
 *
 */

:- module(frogs_improved_problem, [ initial_state/1
                                  , final_state/1
                                  , operation/4
                                  , frogs_pretty_solution/2
                                  , frogs_pretty_animation/2
                                  ]).

%! initial_state(-S:  atom) is det.
%
% An instance of problem to solve as a list of frogs, either "green" or "red", and spaces (actually water lilies) between and around them.
%
% @arg S  A state, as a list of atoms, with only "red," "green," or "space" as values.
%
initial_state([green, green, green, space, red, red, red]).
% initial_state([green, green, space, space, space, space, space, space, space, red]).
% initial_state([green, green, space, space, space, space, space, space, red]).

:- begin_tests(frogs_improved_initial_state).

test(list_of_authorised_atoms) :-
   assertion( forall( ( initial_state(I)
                      , member(X, I)
                      )
                    , member(X, [green, red, space])
                    ) ).

:- end_tests(frogs_improved_initial_state).

%! final_state(?S:  list(atom)) is det.
%
% The final state(s) of the crossing frogs problem.
%
% @arg S  A state.
%
final_state([red, red, red, space, green, green, green]).
% final_state([red, space, space, space, space, space, space, space, green, green]).
% final_state([red, space, space, space, space, space, space, green, green]).

:- begin_tests(frogs_improved_final_state).

test(list_of_authorised_atoms) :-
   assertion( forall( ( final_state(S)
                      , member(X, S)
                      )
                    , member(X, [green, red, space])
                    ) ).

:- end_tests(frogs_improved_final_state).

%! operation(-D:  description, +S:  state, -NS:  state, -C:  cost) is det.
%
% Four operations can be applied to a state:
%    * moving a red frog from left to right;
%    * moving a green one from right to left;
%    * having a red frog jump over a green one; 
%    * having a green frog jump over a red one. 
%
% @arg D   The description of the applied operation, possibly with it "curried" parameters.
% @arg S   The current state.
% @arg NS  The new state obtained by applying the described operation to the current state.
% @arg C   The cost of this operation, either 1 for advancing or 2 for jumping, i.e., the number of "steps".
%
operation(jump_red, S, NS, 2) :-
   findall((S, NS), sub_operation(jump_red, S, NS, _), OS),
   last(OS, (S, NS)).                    % The improvement consists in considering only one red frog...
operation(move_red, S, NS, 1) :-
   findall((S, NS), sub_operation(move_red, S, NS, _), OS),
   last(OS, (S, NS)).
operation(jump_green, S, NS, 2) :-
   sub_operation(jump_green, S, NS, _),
   !.                                    % ... and only one green frog...
operation(move_green, S, NS, 1) :-
   sub_operation(move_green, S, NS, _),
   !.                                    % ... for each king of operation, relying on the older version as a sub-operation.

sub_operation(jump_red, S, NS, 2) :-
   append(L, [space, green, red   | R], S),   % if the pre-condition is satisfied...
   append(L, [red,   green, space | R], NS).  % ... then apply the jump of the red frog over a green one.
sub_operation(move_red, S, NS, 1) :-
   append(L, [space, red   | R], S),
   append(L, [red,   space | R ], NS).
sub_operation(jump_green, S, NS, 2) :-
   append(L, [green, red, space | R], S),
   append(L, [space, red, green | R], NS).
sub_operation(move_green, S, NS, 1) :-
   append(L, [green, space | R], S),
   append(L, [space, green | R], NS).

%! frogs_pretty_solution(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is nondet.
%
% Displays a solution as sequences of visual states from the initial one to a final one, line by line.
%
% @arg I    The initial state.
% @arg SS   One of the solutions associated to I.
%
frogs_pretty_solution(I, SS) :-
   nl,
   pretty_state(I),
   nl,
   forall( member(transition(_, S, _), SS)
         , ( pretty_state(S)
           , nl
           )
         ),
   nl.

%! frogs_pretty_animation(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is nondet.
%
% Similar to 'frogs_pretty_solution/2' but _animates_ the found solutions.
%
% @arg I    The initial state.
% @arg SS   One of the solutions associated to I.
%
frogs_pretty_animation(I, SS) :-
   nl,
   pretty_state(I),
   nl,
   flush_output,
   sleep(0.5),
   forall( member(transition(_, S, _), SS)
         , ( pretty_state(S)
           , nl
           , flush_output
           , sleep(0.5)
           )
         ),
   nl.

%! pretty_state(+S:  list(atom)) is det.
%
% Displays a state of the crossing frogs problem.
%
pretty_state(S) :-
   forall( member(X, S)
         , pretty_frog(X)
         ).

%! pretty_element(+X:  atom) is det.
%
% Displays one of the elements of the puzzle, i.e., a frog or a water lily, using ANSI sequences.
%
pretty_frog(X) :-
   ansi_colour_symbol(X, C, H),
   ansi_format(fg(C), "~w ", [H]).

%! ansi_colour_symbol(?X:  atom,  ?C:  atom, ?H:  char) is nondet.
%
% Characters with colours used in an ANSI terminal to display or animate a solution.
%
% @arg X  The atom used in the problem, i.e., "green" or "red" for the frog plus "space."
% @arg C  The corresponding colour, obviously "green" and "red" for the corresponding frogs.
% @arg H  The character used to represent the atoms.
%
ansi_colour_symbol(green, green, 'S').  % ANSI escape sequences for console colouring
ansi_colour_symbol(red,   red,   'Z').
ansi_colour_symbol(space, blue,  '_').

:- run_tests.

