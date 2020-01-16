% gvim:  fileencoding=utf8

/** <module> Dummy Module to Load all the Directory at Once

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.1

__ALWAYS DOCUMENT AND TEST YOUR CODE!__

Documentation and tests are always much longer than your actual code...
Never neglect the usefulness of the latter with an untyped (or dynamically typed) language, especially when changing some line of code here and there.

To see the documentation of this and all the other modules:
   * start the interactive interpreter at the shell prompt:  "swipl";
   * run the HTTP documentation server: "doc_server(4000).";
   * load this module:  "consult('all.pl').";
   * open (a new tab on) your Web browser (javascript enabled):  "doc_browser.".
   * "Et voilà!"

You can have a look either only at the documentation (default view) or at the formatted code (click on the `(:-)` icons).
Actually, the latter is _strongly_ recommended for viewing the source codes.

__HISTORY__

   * January 2019:  First version
   * March 2019:  Reorganisation of the whole directory
   * June 2019:  Adding the solution checking and overall cost computation on all the problem solvers

__USAGE__
  
To be loaded especially in order to view the whole documentation at once.

*/

:- module(all, [ solve_all/0
               , frogs_solve/0
               , frogs_solve_and_animate/0
               , bridge_solve/0
               , jugs_solve_naive/0
               , jugs_solve_directed_cycle_detection/0
               , jugs_solve_depth_limit/1
               , jugs_solve_depth_limit_with_tracing/2
               , jugs_solve_depth_limit_with_tracing/1
               , jugs_solve_depth_limit_with_tracing/0
               , jugs_solve_iterated_deepening/1
               , jugs_solve_uninformed_branch_and_bound/0
               , jugs_solve_memoing/0
               , puzzle8_solve/1
               , puzzle8_solve/0
               , puzzle15_solve/1
               , puzzle15_solve/0
               ]).

% The Various Inference Engines, with or without Tracing
%
:- use_module('solver_common.pl',                                 [ is_solution/4
                                                                  , solution_cost/2
                                                                  , pretty_solution/2
                                                                  ]).
:- use_module('solver_naive.pl',                                  [ solver/4 as solver_naive
                                                                  ]).
:- use_module('solver_naive_with_tracing.pl',                     [ solver/4 as solver_naive_with_tracing
                                                                  ]).
:- use_module('solver_directed_cycle_detection.pl',               [ solver/4 as solver_directed_cycle_detection
                                                                  ]).
:- use_module('solver_directed_cycle_detection_with_tracing.pl',  [ solver/4 as solver_directed_cycle_detection_with_tracing
                                                                  ]).
:- use_module('solver_depth_limit.pl',                            [ solver/4 as solver_depth_limit
                                                                  , solver/5 as solver_depth_limit
                                                                  ]).
:- use_module('solver_depth_limit_with_tracing.pl',               [ solver/4 as solver_depth_limit_with_tracing
                                                                  , solver/5 as solver_depth_limit_with_tracing
                                                                  , solver/6 as solver_depth_limit_with_tracing
                                                                  ]).
:- use_module('solver_iterated_deepening.pl',                     [ solver/4 as solver_iterated_deepening
                                                                  , solver/5 as solver_iterated_deepening
                                                                  ]).
:- use_module('solver_iterated_deepening_with_tracing.pl',        [ solver/4 as solver_iterated_deepening_with_tracing
                                                                  , solver/5 as solver_iterated_deepening_with_tracing
                                                                  ]).
:- use_module('solver_uninformed_branch_and_bound.pl',            [ solver/4 as solver_uninformed_branch_and_bound
                                                                  , solver/5 as solver_uninformed_branch_and_bound
                                                                  ]).
:- use_module('solver_informed_branch_and_bound.pl',              [ solver/4 as solver_informed_branch_and_bound
                                                                  , solver/5 as solver_informed_branch_and_bound
                                                                  , solver/6 as solver_informed_branch_and_bound
                                                                  , no_evaluation_heuristic/2
                                                                  ]).
:- use_module('solver_informed_branch_and_bound_with_tracing.pl', [ solver/4                  as solver_informed_branch_and_bound_with_tracing
                                                                  , solver/5                  as solver_informed_branch_and_bound_with_tracing
                                                                  , solver/6                  as solver_informed_branch_and_bound_with_tracing
                                                                  , no_evaluation_heuristic/2 as no_evaluation_heuristic_with_tracing
                                                                  ]).
:- use_module('solver_memoing.pl',                                [ solver/4 as solver_memoing
                                                                  ]).
:- use_module('solver_memoing_with_tracing.pl',                   [ solver/4 as solver_memoing_with_tracing
                                                                  ]).
 
% The Crossing Frogs Problem...
%
:- use_module('frogs_problem.pl', [ initial_state/1 as frogs_initial_state
                                  , final_state/1   as frogs_final_state
                                  , operation/4     as frogs_operation
                                  , frogs_pretty_solution/2
                                  , frogs_pretty_animation/2
                                  ]).

% ... and its Solution
%
:- use_module('frogs_solution.pl', [ solve/0             as frogs_solve
                                   , solve_and_animate/0 as frogs_solve_and_animate
                                   ]).

% Again, the Crossing Frogs Problem, in an improved version, ...
%
:- use_module('frogs_improved_problem.pl', [ initial_state/1          as frogs_improved_initial_state
                                           , final_state/1            as frogs_improved_final_state
                                           , operation/4              as frogs_improved_operation
                                           , frogs_pretty_solution/2  as frogs_improved_pretty_solution
                                           , frogs_pretty_animation/2 as frogs_improved_pretty_animation
                                           ]).

% ... and its Solution (which is exactly the same except for one import)
%
:- use_module('frogs_improved_solution.pl', [ solve/0             as frogs_improved_solve
                                            , solve_and_animate/0 as frogs_improved_solve_and_animate
                                            ]).

% The Bridge Crossing Problem...
%
:- use_module('bridge_problem.pl', [ initial_state/1 as bridge_initial_state
                                   , final_state/1   as bridge_final_state
                                   , operation/4     as bridge_operation
                                   , bridge_pretty_solution/2
                                   ]).

% ... and its Solution
%
:- use_module('bridge_solution.pl', [ solve/0 as bridge_solve ]).

% The Filling Jugs Problem...
%
:- use_module('jugs_problem.pl', [ initial_state/1 as jugs_initial_state
                                 , final_state/1   as jugs_final_state
                                 , operation/4     as jugs_operation
                                 , jugs_pretty_solution/2
                                 ]).

% ... and its Numerous Solutions
%
:- use_module('jugs_solution_naive.pl',                                    [ solve/0 as jugs_solve_naive ]).
:- use_module('jugs_solution_directed_cycle_detection.pl',                 [ solve/0 as jugs_solve_directed_cycle_detection ]).
:- use_module('jugs_solution_depth_limit.pl',                              [ solve/1 as jugs_solve_depth_limit ]).
:- use_module('jugs_solution_depth_limit_with_tracing.pl',                 [ solve/2 as jugs_solve_depth_limit_with_tracing
                                                                           , solve/1 as jugs_solve_depth_limit_with_tracing
                                                                           , solve/0 as jugs_solve_depth_limit_with_tracing
                                                                           ]).
:- use_module('jugs_solution_iterated_deepening.pl',                       [ solve/1 as jugs_solve_iterated_deepening]).
:- use_module('jugs_solution_uninformed_branch_and_bound.pl',              [ solve/0 as jugs_solve_uninformed_branch_and_bound ]).
:- use_module('jugs_solution_informed_branch_and_bound.pl',                [ solve/0 as jugs_solve_informed_branch_and_bound ]).
:- use_module('jugs_solution_memoing.pl',                                  [ solve/0 as jugs_solve_memoing]).

% The Puzzle 8 Problem...
%
:- use_module('puzzle8_problem.pl', [ initial_state/1 as puzzle8_initial_state
                                    , final_state/1   as puzzle8_final_state
                                    , operation/4     as puzzle8_operation
                                    , puzzle8_pretty_solution/2
                                    ]).

% ... and its Solution
%
:- use_module('puzzle8_solution.pl', [ solve/1 as puzzle8_solve
                                     , solve/0 as puzzle8_solve
                                     ]).

% The Puzzle 15 Problem...
%
:- use_module('puzzle15_problem.pl', [ initial_state/1 as puzzle15_initial_state
                                     , final_state/1   as puzzle15_final_state
                                     , operation/4     as puzzle15_operation
                                     , puzzle15_pretty_solution/2
                                     ]).

% ... and its Solution
%
:- use_module('puzzle15_solution.pl', [ solve/1 as puzzle15_solve
                                      , solve/0 as puzzle15_solve
                                      ]).

%! solve_all is det.
%
% Utility predicate to run at once one or several solvers on each of a few problem instances.
%
% The running time of each solver is timed in the number of inferences as well as clock-wall time.
% It is possible to compare the performances on:
%
%    * the various solvers applied to the filling jugs problem (although they do not find the same first solution),
%
%    * the two variants of the crossing frogs problem (where the improved version -- drastically -- reduces the number of alternatives),
%
%    * the two variants of the same puzzle, of size 8 and 15 respectively (i.e., the exponential increase of search time with respect to a small increase in problem size).
%
% @tbd  By the way, why do not you try Puzzle 24?
%
% However, take them with a grain of salt.
% In particular, they incorporate not only the actual solving time but also the pretty-printing, the solution check, and the overall cost computation.
% Above all, none of the solvers has been particularly optimised.
%
solve_all :-
   write("SOLVING THE CROSSING FROGS PROBLEM"), nl,
   write("-- Naïve"), nl,
   time(frogs_solve),
   write("SOLVING THE CROSSING FROGS PROBLEM IN AN IMPROVED VERSION"), nl,
   write("-- Naïve"), nl,
   time(frogs_improved_solve),
   write("SOLVING THE BRIDGE CROSSING RIDDLE"), nl,
   write("-- Naïve"), nl,
   time(bridge_solve),
   write("SOLVING THE FILLING JUGS PUZZLE (IN VARIOUS WAYS)"), nl,
   write("-- Naïve?  No!"), nl,
   write("-- Directed-cycle detection"), nl,
   time(jugs_solve_directed_cycle_detection),
   write("-- Depth limit (set to 10)"), nl,
   time(jugs_solve_depth_limit(10)),
   write("-- Iterated deepening (maximal depth set to 10)"), nl,
   time(jugs_solve_iterated_deepening(10)),
   write("-- Uninformed branch-and-bound"), nl,
   time(jugs_solve_uninformed_branch_and_bound),
   write("-- (Uniformly) informed branch-and-bound"), nl,
   time(jugs_solve_informed_branch_and_bound),
   write("-- Memoïng"), nl,
   time(jugs_solve_memoing),
   write("SOLVING THE PUZZLE 8 PROBLEM"), nl,
   write("-- Informed branch-and-bound (plus directed-cycle detection)"), nl,
   time(puzzle8_solve(20)),
   write("SOLVING THE PUZZLE 15 PROBLEM"), nl,
   write("-- Informed branch-and-bound (plus directed-cycle detection)"), nl,
   time(puzzle15_solve(20)),
   write("ONLY ONCE EACH!"), nl,
   !.

