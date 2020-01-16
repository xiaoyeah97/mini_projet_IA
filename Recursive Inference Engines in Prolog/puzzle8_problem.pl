% gvim:  fileencoding=utf8

/** <module> The Puzzle 8 Problem

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.0

__HISTORY__

 * March 2019:  first version

__CONTENTS__

Following the recommended methodology, the description of the solution to a problem exports exactly three predicates:
   * 'initial_state/1', that provides the description of an (example) initial problem (possibly several);
   * 'final_state/1', that verifies if a given state is a solution to the problem;
   * 'operation/4', that applies an authorised transition to a given state, giving a new state for a given cost of the transition.

__RIDDLE__


 */

:- module(puzzle8_problem, [ initial_state/1
                           , final_state/1
                           , operation/4
                           , hamming_heuristic/2
                           , puzzle8_pretty_solution/2
                           ]).
   
%! initial_state(-S:  state) is det.
%
% @arg S  A state, as a list of atoms, where "x" stands for the empty place.
%
initial_state(state([ d, c, e
                    , h, x, a
                    , b, g, f
                    ])).

%! final_state(?S:  state) is det.
%
% @arg S  A state.
%
final_state(state([ a, b, c
                  , d, e, f
                  , g, h, x
                  ])).

%! operation(-D:  description, +S:  state, -NS:  state, -C:  cost) is det.
%
% Actually, a single operation but with multiple instantiations is possible:  Moving the empty place to an adjacent place.
% Therefore, the empty place can go up, down, left, or right.
%
% @arg D   The description of the applied operation, possibly with it "curried" parameters.
% @arg S   The current state.
% @arg NS  The new state obtained by applying the described operation to the current state.
% @arg C   The cost of this operation, always 1.
%
operation(up, state([ A, B, C
                    , x, E, F
                    , G, H, I
                    ]), state([ x, B, C
                              , A, E, F
                              , G, H, I
                              ]), 1). % Ugly but rather fast implementation!
operation(up, state([ A, B, C
                    , D, x, F
                    , G, H, I
                    ]), state([ A, x, C
                              , D, B, F
                              , G, H, I
                              ]), 1).
operation(up, state([ A, B, C
                    , D, E, x
                    , G, H, I
                    ]), state([ A, B, x
                              , D, E, C
                              , G, H, I
                              ]), 1).
operation(up, state([ A, B, C
                    , D, E, F
                    , x, H, I
                    ]), state([ A, B, C
                              , x, E, F
                              , D, H, I
                              ]), 1).
operation(up, state([ A, B, C
                    , D, E, F
                    , G, x, I
                    ]), state([ A, B, C
                              , D, x, F
                              , G, E, I
                              ]), 1).
operation(up, state([ A, B, C
                    , D, E, F
                    , G, H, x
                    ]), state([ A, B, C
                              , D, E, x
                              , G, H, F
                              ]), 1).
operation(down, state([ x, B, C
                      , D, E, F
                      , G, H, I
                      ]), state([ D, B, C
                                , x, E, F
                                , G, H, I
                                ]), 1).
operation(down, state([ A, x, C
                      , D, E, F
                      , G, H, I
                      ]), state([ A, E, C
                                , D, x, F
                                , G, H, I
                                ]), 1).
operation(down, state([ A, B, x
                      , D, E, F
                      , G, H, I
                      ]), state([ A, B, F
                                , D, E, x
                                , G, H, I
                                ]), 1).
operation(down, state([ A, B, C
                      , x, E, F
                      , G, H, I
                      ]), state([ A, B, C
                                , G, E, F
                                , x, H, I
                                ]), 1).
operation(down, state([ A, B, C
                      , D, x, F
                      , G, H, I
                      ]), state([ A, B, C
                                , D, H, F
                                , G, x, I
                                ]), 1).
operation(down, state([ A, B, C
                      , D, E, x
                      , G, H, I
                      ]), state([ A, B, C
                                , D, E, I
                                , G, H, x
                                ]), 1).
operation(left, state([ A, x, C
                      , D, E, F
                      , G, H, I
                      ]), state([ x, A, C
                                , D, E, F
                                , G, H, I
                                ]), 1).
operation(left, state([ A, B, C
                      , D, x, F
                      , G, H, I
                      ]), state([ A, B, C
                                , x, D, F
                                , G, H, I
                                ]), 1).
operation(left, state([ A, B, C
                      , D, E, F
                      , G, x, I
                      ]), state([ A, B, C
                                , D, E, F
                                , x, G, I
                                ]), 1).
operation(left, state([ A, B, x
                      , D, E, F
                      , G, H, I
                      ]), state([ A, x, B
                                , D, E, F
                                , G, H, I
                                ]), 1).
operation(left, state([ A, B, C
                      , D, E, x
                      , G, H, I
                      ]), state([ A, B, C
                                , D, x, E
                                , G, H, I
                                ]), 1).
operation(left, state([ A, B, C
                      , D, E, F
                      , G, H, x
                      ]), state([ A, B, C
                                , D, E, F
                                , G, x, H
                                ]), 1).
operation(right, state([ x, B, C
                       , D, E, F
                       , G, H, I
                       ]), state([ B, x, C
                                 , D, E, F
                                 , G, H, I
                                 ]), 1).
operation(right, state([ A, B, C
                       , x, E, F
                       , G, H, I
                       ]), state([ A, B, C
                                 , E, x, F
                                 , G, H, I
                                 ]), 1).
operation(right, state([ A, B, C
                       , D, E, F
                       , x, H, I
                       ]), state([ A, B, C
                                 , D, E, F
                                 , H, x, I
                                 ]), 1).
operation(right, state([ A, x, C
                       , D, E, F
                       , G, H, I
                       ]), state([ A, C, x
                                 , D, E, F
                                 , G, H, I
                                 ]), 1).
operation(right, state([ A, B, C
                       , D, x, F
                       , G, H, I
                       ]), state([ A, B, C
                                 , D, F, x
                                 , G, H, I
                                 ]), 1).
operation(right, state([ A, B, C
                       , D, E, F
                       , G, x, I
                       ]), state([ A, B, C
                                 , D, E, F
                                 , G, I, x
                                 ]), 1).

:- begin_tests(puzzle8_operation).

test(no_more_than_four_moves) :-
   assertion( forall( ( initial_state(S)
                      ; final_state(S)
                      )
                    , ( findall( NS
                               , operation(_, S, NS, _)
                               , NSS)
                      , length(NSS, N_NSS)
                      , N_NSS =< 4
                      )
                    ) ).

test(a_single_difference_from_final_state) :-
   final_state(F),
   assertion( forall( operation(_, F, NS, _)
                    , hamming_heuristic(NS, 1)
                    ) ).

:- end_tests(puzzle8_operation).

%! puzzle8_pretty_solution(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is nondet.
%
% Displays a solution as sequences of visual states from the initial one to a final one, line by line.
%
% @arg I    The initial state.
% @arg SS   One of the solutions associated to I.
%
puzzle8_pretty_solution(I, SS) :-
   nl,
   pretty_state(I),
   forall( member(transition(O, S, _), SS)
         , ( pretty_move(O)
           , pretty_state(S)
           )
         ),
   nl.

%! pretty_state(+S:  list(atom)) is det.
%
% Displays a state of the crossing puzzle8 problem.
%
pretty_state(state([ A, B, C
                   , D, E, F
                   , G, H, O
                   ])) :-
   format("+---+---+---+\n"),
   puzzle_line(A, B, C),
   format("+---+---+---+\n"),
   puzzle_line(D, E, F),
   format("+---+---+---+\n"),
   puzzle_line(G, H, O),
   format("+---+---+---+\n").
   
puzzle_line(A, B, C) :-
   format("| ", []),
   pretty_character(A),
   format(" | ", []),
   pretty_character(B),
   format(" | ", []),
   pretty_character(C),
   format(" |", []),
   nl.
   
%! pretty_element(+X:  atom) is det.
%
% Displays one of the elements of the puzzle, i.e., a frog or a water lily, using ANSI sequences.
%
pretty_character(X) :-
   ansi_colour_symbol(X, C, H),
   ansi_format(fg(C), "~w", [H]).

%! ansi_colour_symbol(?X:  atom,  ?C:  atom, ?H:  char) is nondet.
%
% Characters with colours used in an ANSI terminal to display or animate a solution.
%
% @arg X  The atom used in the problem, i.e., "green" or "red" for the frog plus "space."
% @arg C  The corresponding colour, obviously "green" and "red" for the corresponding puzzle8.
% @arg H  The character used to represent the atoms.
%
ansi_colour_symbol(a, red,    'A').  % ANSI escape sequences for console colouring
ansi_colour_symbol(b, red,    'B').
ansi_colour_symbol(c, red,    'C').
ansi_colour_symbol(d, yellow, 'D').
ansi_colour_symbol(e, yellow, 'E').
ansi_colour_symbol(f, yellow, 'F').
ansi_colour_symbol(g, green,  'G').
ansi_colour_symbol(h, green,  'H').
ansi_colour_symbol(x, green,  ' ').
ansi_colour_symbol(up,    blue, '▼'). % the opposite since we virtually move the blank cell rather than the one with the letter
ansi_colour_symbol(down,  blue, '▲').
ansi_colour_symbol(left,  blue, '▶').
ansi_colour_symbol(right, blue, '◀').

%! pretty_move(+O:  term) is det.
%
% Displays a representation of the transitions
%
pretty_move(O) :-
   write("      "),
   pretty_character(O),
   nl.

%! hamming_heuristic(+S:  state,  ?HC:  int) is det.
%
% This heuristic returns the number of elements at the wrong position.
%
% @arg S    The state.
% @arg HC   The heuristic cost to a solution, by default.
%
hamming_heuristic(state(S), HC) :-
   final_state(state(F)),
   maplist(difference, S, F, D),
   sum_list(D, HC_2),
   HC is div(HC_2, 2). % since each difference is counted twice

difference(X, Y, 0) :-
   X = Y.
difference(X, Y, 1) :-
   X \= Y.

:- begin_tests(puzzle8_hamming_heuristic).

test(null_for_final_state) :-
   final_state(F),
   assertion( hamming_heuristic(F, 0) ).

test(non_negative_for_initial_state) :-
   initial_state(I),
   hamming_heuristic(I, HC),
   assertion( HC >= 0 ).

:- end_tests(puzzle8_hamming_heuristic).

:- run_tests.

