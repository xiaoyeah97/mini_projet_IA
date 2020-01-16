:- module(solution, [solve/0]).

:- use_module(cobicyclage_conception, [ initial_state/1
                                  , final_state/1
                                  , operation/4
                                  , cobicyclage_pretty_solution/2
                                  ]).

:- use_module(solver_common, [ is_solution/4
                             , solution_cost/2
                             ]).    

:- use_module(solver_informed_branch_and_bound, [ solver/5
                                                , solver/6
                                                , no_evaluation_heuristic/2
                                                ]).

solve :-
   initial_state(I),
   writef("Initial state:  %t\n", [I]),
%    jugs_max_bound(B),
%    writef("Max bound:  %t\n", [B]),
   solver(final_state, operation, no_evaluation_heuristic, I, S),
   writef("Solution:  %t\n", [S]),
   cobicyclage_pretty_solution(I, S),
   writef("i.e., %t\n", [S]),
%    is_solution(jugs_problem:final_state, jugs_problem:operation, I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).

%! jugs_max_bound(?B:  int) is det.
%
% Some maximal liters of water to be used in order to solve the jug problem.
%
% jugs_max_bound(1000).