:- module(solution, [solve/0]).

:- use_module(cobicyclage_conception, [ initial_state/1
                                  , final_state/1
                                  , operation/4
                                  , cobicyclage_pretty_solution/2
                                  ]).

:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).                

:- use_module(solver_naive, [ solver/4 ]).

solve :-
   initial_state(I),
   solver(final_state, operation, I, S),
   cobicyclage_pretty_solution(I, S),
%    writef("i.e., %t\n", [S]),
%    is_solution(jugs_problem:final_state, jugs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).