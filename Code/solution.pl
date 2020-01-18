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

:- use_module(cobicyclage_analyse, [ troncon/3
                               , etape/5
                               , parcours_reel/5
                               , parcours_virtuel/4
                               , personne_mobile/4
                               , proprietaire/6
                               , usager/4
                               , duree/1
                               , horaire/1
                               ]).

solve :-
   initial_state(I),
   solver(final_state, operation, heuristic_dij, I, S),
   writef("Solution:  %t\n", [S]),
   cobicyclage_pretty_solution(I, S),
   solution_cost(S, C),
   writef("for a total cost of %t.\n", [C]).


heuristic_dij(state(VS,RS), Cout) :-
   c_vs(VS, Cout).


c_vs([], 0).
c_vs([virtuel(S, D, X, H_max_X)|VS], Cout_V):-
   dijkstra(D, X, ShortestPath, Cout_L),
   c_vs(VS, Cout_VS),
   Cout_V is Cout_VS + Cout_L.




% Dijkstra
%   + Start        : Point de départ
%   + Finish       : Point de d'arrivée
%   - ShortestPath : Chemin le plus court
%   - Len          : Longueur de ce chemin
%


dijkstra(Start, Finish, ShortestPath, Len) :-
  dijk( [0-[Start]], Finish, RShort, Len),
  reverse(RShort, ShortestPath).



% Le dernier point visité est le point d'arrivée => on s'arrête
%

dijk( [ Len-[Fin|RPath] |_], Fin, [Fin|RPath], Len) :- !.


dijk( Visited, Fin, RShortestPath, Len) :-
  % Recherche du meilleur candidat (prochain point à ajouter au graphe)
  %   et appel récursif au prédicat
  %

  bestCandidate(Visited, BestCandidate),
  dijk( [BestCandidate|Visited], Fin, RShortestPath, Len).



%
% Recherche toutes les arrêtes pour lesquelles on a:
%  - un point dans le graphe (visité)
%  - un point hors du graphe (candidat)
%
% Retourne le point qui minimise la distance par rapport à l'origine
%


bestCandidate(Paths, BestCandidate) :-

  % à partir d'un point P1 :

  findall(
     NP            % on fait la liste de tous les points P2 tels que:
  ,
    (
      member( Len-[P1|Path], Paths),  % - le point P2 a déjà été visité
      troncon(P1,P2,Dist),                % - il existe un arc allant de P1 à P2, de distance Dist
      \+isVisited(Paths, P2),         % - le point P2 n'a pas encore été visité

      NLen is Len+Dist,               % on calcule la distance entre l'origine et le point P2

      NP=NLen-[P2,P1|Path]            % on met chaque élément de la liste sous la forme: Distance-Chemin
                                      % pour pouvoir les trier avec le prédicat keysort/2
    )
  ,
    Candidates
  ),

  % On trie et on retient le chemin le plus court
  minimum(Candidates, BestCandidate).



%
% Sort le meilleur candidat parmi une liste de candidats
% (= celui de chemin le moins long)
%

minimum(Candidates, BestCandidate) :-
  keysort(Candidates, [BestCandidate|_]).



%
% Teste si un point P a déjà été visité
%

isVisited(Paths, P) :-
  memberchk(_-[P|_], Paths).
