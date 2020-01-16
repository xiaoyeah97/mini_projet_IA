% gvim:  fileencoding=utf8

/** <module> The Bridge Crossing Problem

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.0

__HISTORY__

 * March 2019:  first version
 *
__CONTENTS__

Following the recommended methodology, the description of the solution to a problem exports exactly three predicates:
   * 'initial_state/1', that provides the description of an (example) initial problem (possibly several);
   * 'final_state/1', that verifies if a given state is a solution to the problem;
   * 'operation/4', that applies an authorised transition to a given state, giving a new state for a given cost of the transition.

__RIDDLE__

Cf. [["Can you solve the bridge riddle?" by Alex Gendler at TEDed][https://ed.ted.com/lessons/can-you-solve-the-bridge-riddle-alex-gendler]]

 */

:- module(bridge_problem, [ initial_state/1
                          , final_state/1
                          , operation/4
                          , bridge_pretty_solution/2
                          ]).
   
%! character_time(+C:  atom,  -T:  int) is semidet.
%! character_time(?C:  atom,  ?T:  int) is nondet.
%
% An instance of problem to solve as a list of bridge, either "green" or "red", and spaces (actually water lilies) between and around them.
%
% @arg C  One of the characters of the riddle.
% @arg T  The times that it takes him or her to cross the bridge, either way.
%
character_time(professor,  10).
character_time(assistant,   2).
character_time(phd_student, 1).
character_time(engineer,    5).

%! initial_state(-S:  atom) is det.
%
% On the initial state, all the characters are located on the entry side of the bridge and the time counter has been set to zero.
%
% @arg S  A state, as a list of atoms, with only "red," "green," or "space" as values.
%
initial_state(state(0, [professor, assistant, phd_student, engineer], [], input)).

%! final_state(?S:  list(atom)) is det.
%
% On a final state, there remains no character on the input side of the bridge and the elapsed time is at most 17 minutes.
%
% @arg S  A state.
%
final_state(state(T, [], _, output)) :-
   T =< 17.

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
operation(go(X, Y), state(T, PI, PO, input), state(T2, PI2, [X, Y | PO], output), MT) :-
   member(X, PI),
   member(Y, PI),
   X @< Y, % < rather than only \= to avoid differentiating between (X, Y) and (Y, X)
   character_time(X, TX),
   character_time(Y, TY),
   MT is max(TX, TY),
   plus(T, MT, T2),
   T2 =< 17,
   subtract(PI, [X, Y], PI2).
operation(return(X), state(T, PI, PO, output), state(T2, [X | PI], PO2, input), TX) :-
   member(X, PO),
   character_time(X, TX),
   plus(T, TX, T2),
   T2 =< 17,
   delete(PO, X, PO2).

%! bridge_pretty_solution(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is nondet.
%
% Displays a solution as sequences of visual states from the initial one to a final one, line by line.
%
% @arg I    The initial state.
% @arg SS   One of the solutions associated to I.
%
bridge_pretty_solution(I, SS) :-
   nl,
   pretty_state(I),
   nl,
   forall( member(transition(O, S, _), SS)
         , ( pretty_state(S)
           , write("\t")
           , pretty_move(O)
           , nl
           )
         ),
   nl.

%! pretty_state(+S:  list(atom)) is det.
%
% Displays a state of the crossing bridge problem.
%
pretty_state(state(T, PI, PO, S)) :-
   writef("%4r:\t", [T]),
   forall( member(C, PI)
         , pretty_character(C)
         ),
   ( S = input
   -> pretty_character(lamp)
   ;  true
   ),
   write(".........."),
   ( S = output
   -> pretty_character(lamp)
   ;  true
   ),
   forall( member(C, PO)
         , pretty_character(C)
         ).

%! pretty_element(+X:  atom) is det.
%
% Displays one of the elements of the puzzle, i.e., a frog or a water lily, using ANSI sequences.
%
pretty_character(X) :-
   ansi_colour_symbol(X, C, H),
   ansi_format(fg(C), "~w ", [H]).

%! ansi_colour_symbol(?X:  atom,  ?C:  atom, ?H:  char) is nondet.
%
% Characters with colours used in an ANSI terminal to display or animate a solution.
%
% @arg X  The atom used in the problem, i.e., "green" or "red" for the frog plus "space."
% @arg C  The corresponding colour, obviously "green" and "red" for the corresponding bridge.
% @arg H  The character used to represent the atoms.
%
ansi_colour_symbol(professor,   green,  'P').  % ANSI escape sequences for console colouring
ansi_colour_symbol(assistant,   red,    'A').
ansi_colour_symbol(phd_student, blue,   'S').
ansi_colour_symbol(engineer,    white,  'E').
ansi_colour_symbol(lamp,        yellow, '#').

%! pretty_move(+O:  term) is det.
%
% Displays a representation of the transitions
%
pretty_move(go(X, Y)) :-
   write("-> "),
   pretty_character(X),
   pretty_character(Y),
   pretty_character(lamp).
pretty_move(return(X)) :-
   write("<- "),
   pretty_character(lamp),
   pretty_character(X).
