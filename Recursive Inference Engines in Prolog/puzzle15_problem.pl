% gvim:  fileencoding=utf8

/** <module> The Puzzle 15 Problem

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.0

__HISTORY__

 * June 2019:  first version, cloned and adapted from puzzle 8

__CONTENTS__

Following the recommended methodology, the description of the solution to a problem exports exactly three predicates:
   * 'initial_state/1', that provides the description of an (example) initial problem (possibly several);
   * 'final_state/1', that verifies if a given state is a solution to the problem;
   * 'operation/4', that applies an authorised transition to a given state, giving a new state for a given cost of the transition.

__RIDDLE__


 */

:- module(puzzle15_problem, [ initial_state/1
                            , final_state/1
                            , operation/4
                            , hamming_heuristic/2
                            , puzzle15_pretty_solution/2
                            ]).
   
:- use_module(library(clpfd)). % for 'transpose/2'

%! initial_state(-S:  state) is det.
%
% @arg S  A state, as a list of atoms, where "x" stands for the empty cell.
%
initial_state(state([ [a, c, d, h]
                    , [j, b, f, l]
                    , [e, i, x, g]
                    , [m, n, k, o]
                    ])).

%! final_state(?S:  state) is det.
%
% @arg S  A state.
%
final_state(state([ [a, b, c, d]
                  , [e, f, g, h]
                  , [i, j, k, l]
                  , [m, n, o, x]
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
operation(up, state([R0, R1, R2, R3]), state([T0, T1, R2, R3]), 1) :-
   exchange_row_x(R0, R1, T0, T1).
operation(up, state([R0, R1, R2, R3]), state([R0, T1, T2, R3]), 1) :-
   exchange_row_x(R1, R2, T1, T2).
operation(up, state([R0, R1, R2, R3]), state([R0, R1, T2, T3]), 1) :-
   exchange_row_x(R2, R3, T2, T3).

operation(down, state([R0, R1, R2, R3]), state([T0, T1, R2, R3]), 1) :-
   exchange_row_x(R1, R0, T1, T0).
operation(down, state([R0, R1, R2, R3]), state([R0, T1, T2, R3]), 1) :-
   exchange_row_x(R2, R1, T2, T1).
operation(down, state([R0, R1, R2, R3]), state([R0, R1, T2, T3]), 1) :-
   exchange_row_x(R3, R2, T3, T2).

operation(left, state(S), state(NS), C) :- % Slow implementation
   transpose(S, S_T),
   operation(up, state(S_T), state(NS_T), C),
   transpose(NS_T, NS).

operation(right, state(S), state(NS), C) :- % Slower implementation
   maplist(reverse, S, S_R),
   operation(left, state(S_R), state(NS_R), C),
   maplist(reverse, NS_R, NS).

%! exchange_row_x(-F:  description, +S:  state, -NS:  state, -C:  cost) is det.
%
% Actually, a single operation but with multiple instantiations is possible:  Moving the empty place to an adjacent place.
% Therefore, the empty place can go up, down, left, or right.
%
% @arg R   A row
% @arg S   Another row
% @arg T   Row R where the value aligned with the empty cell in row S has been removed
% @arg U   Row S where the empty cell has been replaced by the aligned value in row R
%
exchange_row_x([A, B, C, D],
               [x, F, G, H], [x, B, C, D],
                             [A, F, G, H]). % Ugly but rather fast implementation!
exchange_row_x([A, B, C, D],
               [E, x, G, H], [A, x, C, D],
                             [E, B, G, H]).
exchange_row_x([A, B, C, D],
               [E, F, x, H], [A, B, x, D],
                             [E, F, C, H]).
exchange_row_x([A, B, C, D],
               [E, F, G, x], [A, B, C, x],
                             [E, F, G, D]).

:- begin_tests(puzzle15_operation).

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

:- end_tests(puzzle15_operation).

%! puzzle15_pretty_solution(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is nondet.
%
% Displays a solution as sequences of visual states from the initial one to a final one, line by line.
%
% @arg I    The initial state.
% @arg SS   One of the solutions associated to I.
%
puzzle15_pretty_solution(I, SS) :-
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
% Displays a state of the crossing puzzle15 problem.
%
pretty_state(state([ [A, B, C, D]
                   , [E, F, G, H]
                   , [I, J, K, L]
                   , [M, N, O, P]
                   ])) :-
   format("+---+---+---+---+\n"),
   puzzle_line(A, B, C, D),
   format("+---+---+---+---+\n"),
   puzzle_line(E, F, G, H),
   format("+---+---+---+---+\n"),
   puzzle_line(I, J, K, L),
   format("+---+---+---+---+\n"),
   puzzle_line(M, N, O, P),
   format("+---+---+---+---+\n").
   
puzzle_line(A, B, C, D) :-
   format("| ", []),
   pretty_character(A),
   format(" | ", []),
   pretty_character(B),
   format(" | ", []),
   pretty_character(C),
   format(" | ", []),
   pretty_character(D),
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
% @arg C  The corresponding colour, obviously "green" and "red" for the corresponding puzzle15.
% @arg H  The character used to represent the atoms.
%
ansi_colour_symbol(a, red,    'A').  % ANSI escape sequences for console colouring
ansi_colour_symbol(b, red,    'B').
ansi_colour_symbol(c, red,    'C').
ansi_colour_symbol(d, red,    'D').
ansi_colour_symbol(e, yellow, 'E').
ansi_colour_symbol(f, yellow, 'F').
ansi_colour_symbol(g, yellow, 'G').
ansi_colour_symbol(h, yellow, 'H').
ansi_colour_symbol(i, green,  'I').
ansi_colour_symbol(j, green,  'J').
ansi_colour_symbol(k, green,  'K').
ansi_colour_symbol(l, green,  'L').
ansi_colour_symbol(m, cyan,   'M').
ansi_colour_symbol(n, cyan,   'N').
ansi_colour_symbol(o, cyan,   'O').
ansi_colour_symbol(x, cyan,   ' ').
ansi_colour_symbol(up,    blue, '▼'). % the opposite since we virtually move the blank cell rather than the one with the letter
ansi_colour_symbol(down,  blue, '▲').
ansi_colour_symbol(left,  blue, '▶').
ansi_colour_symbol(right, blue, '◀').

%! pretty_move(+O:  term) is det.
%
% Displays a representation of the transitions
%
pretty_move(O) :-
   write("        "),
   pretty_character(O),
   nl.

%! hamming_heuristic(+S:  state,  ?HC:  int) is det.
%
% This heuristic returns the number of elements at the wrong position.
%
% @arg S    The state.
% @arg HC   The heuristic cost to a solution, by default.
%
hamming_heuristic(state([R0, R1, R2, R3]), HC) :-
   final_state(state([F0, F1, F2, F3])),
   maplist(difference, R0, F0, D0),
   maplist(difference, R1, F1, D1),
   maplist(difference, R2, F2, D2),
   maplist(difference, R3, F3, D3),
   sum_list(D0, HC0),
   sum_list(D1, HC1),
   sum_list(D2, HC2),
   sum_list(D3, HC3),
   HC is div(HC0 + HC1 + HC2 + HC3, 2). % since each difference is counted twice

difference(X, X, 0) :-
   !. % to avoid a choice-point warning during the tests (does not occur with puzzle8!?), ...
difference(_, _, 1). % ... which helps to simplify the next definition

:- begin_tests(puzzle15_hamming_heuristic).

test(null_for_final_state) :-
   final_state(F),
   assertion( hamming_heuristic(F, 0) ).

test(non_negative_for_initial_state) :-
   initial_state(I),
   hamming_heuristic(I, HC),
   assertion( HC >= 0 ).

:- end_tests(puzzle15_hamming_heuristic).

:- run_tests.

