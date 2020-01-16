% gvim:  fileencoding=utf8

/** <module> Résolution du problème du co-bicyclage

@author José Martinez
@see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
@license All Rights Reserved

This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
It is provided only for a pedagogical usage at Polytech Nantes.

@version 1.0

__HISTORY__

 * July 22nd, 2019:  first version

 */

:- module(cobicyclage_conception, [ initial_state/1
                                  , final_state/1
                                  , operation/4
                                  , cobicyclage_pretty_solution/2
                                  ]).

:- use_module(cobicyclage_analyse, [ troncon/3
                                   , etape/5
                                   , parcours_reel/5
                                   , parcours_virtuel/4
                                   ]).
:- use_module(temps, [ horaire_minute/2 ]).

%! initial_state(-S:  atom) is det.
%
% L'état initial est une recopie de l'ensemble parcours virtuels des usagers avec une réalisation de chacun encore vide.
%
initial_state(state(VS, [])) :-
   findall( virtuel(S, D, A, H_max)
          , parcours_virtuel(S, D, A, H_max)
          , VS
          ).  % "Récupération" de tous les parcours virtuels.

%! final_state(?S:  list(atom)) is det.
%
% Un état final est un état dans lequel il n'y a plus aucun parcours virtuel à compléter ou finaliser.
%
final_state(state([], _)).

%! operation(-D:  description, +S:  state, -NS:  state, -C:  cost) is det.
%
% Trois opérations sont possibles :
%
%    * attendre le passage d'un vélo pour se rendre d'un point à un autre plus rapidement, si l'attente n'est pas trop longue et qu'un vélo vient à suivre un même tronçon qu'un usager ;
%
%    * emprunter directement un tronçon à pieds ;
%
%    * partir de chez soi assez tôt !
%
% @arg D   Description de l'opération de transformation appliquée, avec ses éventuels paramètres "currifiés".
% @arg S   L'état courant.
% @arg NS  Le nouvel état après application de l'opération à partir de l'état courant.
% @arg C   Le coût de l'opération en nombre de minutes totales écoulées.
%
operation(a_velo(S, X, Y, P, T, A),
          state(VS, RS),
          state([virtuel(S, D, X, H_max_X) | NVS],
                [reel(S, X, Y)             |  RS]),
          C) :-
   member(V, VS),                     % S'il y a encore un usager devant se rendre
   V = virtuel(S, D, Y, H_max_Y),     % de son domicile à une étape donnée (initialement le lieu d'activité)
   Y \= D,                            % de manière effective,
   etape(P, X, Y, T, H),              % et qu'il existe une étape lui permettant de s'y rendre grâce au vélo d'un propriétaire
   horaire_minute(H_max_Y, T_max_Y),
   plus(T_max_X, T, T_max_Y),
   T_max_X >= 0,                      % qu'il puisse prendre à temps
   horaire_minute(H, T_H),
   plus(T_max_X, A, T_H),
   A >= 0,                            % quitte à attendre,
   \+ member(reel(S, X, _), RS),      % pas encore parcouru,
   \+ member(reel(S, _, X), RS),      % ni dans un sens ni dans l'autre,
   delete(VS, V, NVS),                % alors on peut essayer d'embarquer,
   plus(T_max_X, C, T_max_Y),         % le coût étant la somme du temps de transport à vélo et du temps d'attente du passage de ce vélo.
   horaire_minute(H_max_X, T_max_X).
operation(a_pieds(S, X, Y, T),
          state(VS, RS),
          state([virtuel(S, D, X, H_max_X) | NVS],
                [reel(S, X, Y)             |  RS]),
          T) :-
   member(V, VS),                     % S'il y a encore un usager devant se rendre
   V = virtuel(S, D, Y, H_max_Y),     % de son domicile à une étape donnée (initialement le lieu d'activité)
   Y \= D,                            % de manière effective,
   troncon(X, Y, T),                  % et s'il existe un tronçon de chemin lui permettant de s'y rendre à pieds
   horaire_minute(H_max_Y, T_max_Y),
   T =< T_max_Y,                      % à temps,
   \+ member(reel(S, X, _), RS),      % pas encore parcouru,
   \+ member(reel(S, _, X), RS),      % ni dans un sens ni dans l'autre,
   delete(VS, V, NVS),                % alors on peut l'essayer
   plus(T_max_X, T, T_max_Y),         % en ajustant l'heure d'arrivée au plus tard sur le nœud précédent.
   horaire_minute(H_max_X, T_max_X).
operation(part(S, D, H_max),
          state(VS, RS),
          state(NVS, RS),
          0) :-  % (On pourrait mettre un coût correspondant au temps qu'il faut pour se préparer...)
   member(V, VS),                % Lorsqu'un trajet virtuel restant à parcourir
   V = virtuel(S, D, D, H_max),  % se limite à quitter son domicile
   delete(VS, V, NVS).           % alors c'est fini pour cet usager !

%! cobicyclage_pretty_solution(+I:  State, +SS:  list(transition(D:  description, NS:  State, C:  cost))) is nondet.
%
% Displays a solution as sequences of visual states from the initial one to a final one, line by line.
%
% @arg I    The initial state.
% @arg SS   One of the solutions associated to I.
%
cobicyclage_pretty_solution(_, SS) :-
   nl,
   forall( member(transition(D, _, _), SS)
         , pretty_transition(D)
         ).

%! pretty_transition(+D:  term) is det.
%
% Displays a full textual description of a transition.
%
% @arg D   Formal description of a transition
%
pretty_transition(a_velo(S, X, Y, P, T, A)) :-
   format("L'usager ~w se rend de ~w à ~w sur le vélo de ~w en ~w minutes après ~w minutes d'attente.\n", [S, X, Y, P, T, A]).
pretty_transition(a_pieds(S, X, Y, T)) :-
   format("L'usager ~w se rend de ~w à ~w à pieds en ~w minutes.\n", [S, X, Y, T]).
pretty_transition(part(S, D, H_max)) :-
   format("L'usager ~w part de son domicile ~w à ~w au plus tard\n", [S, D, H_max]).

