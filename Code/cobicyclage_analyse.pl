% gvim:  fileencoding=utf8

/** <module> Analyse du problème du co-bicyclage
 *
 * Le modèle relationnel de données est fourni sur le graphe sémantique ci-dessous.
 * Quelques éléments dérivables sont indiqués (vues relationnelles).
 *
 * ==
 *                     +-----------------------------------+
 *                     | Tronçons                          |
 *                     +-----------------------------------+
 *                     | Origine, Destination              |
 *                     +-----------------------------------+
 *                     | Origine : atom                    |
 *                     | Destination : atom                |
 *                     | Durée piétonnière : natural > 0   |
 *                     +-----------------------------------+
 *                      ^                      ^     ^
 *                      :                      :     :
 *                      :  Domicile <= Origine :     : Lieu d'activité <= Destination
 *                      :                      :     :
 *                      :                     +--------------------------+
 *                      :                     | Personne mobile          |
 *                      :                     +--------------------------+
 *                      :                     | Personne mobile          |
 *                      :                     +--------------------------+
 *                      :                     | Personne mobile : atom   |
 *                      :                     | Domicile : noeud         |
 *                      :                     | Lieu activité : noeud    |
 *                      :                     | Heure maximale : heure   |
 *                      :                     | Minute maximale : minute |
 *                      :                     +--------------------------+
 *                      :                                 ^            ^ 
 *                      :                                 :            :
 *                      : Propriétaire <= Personne mobile :            : Usager <= Personne mobile
 *                      :                                 :            :
 *                      :                                 :            :
 *                      : +-------------------------------------+   +--------------------------+
 *                      : | Propriétaires                       |   | Usagers                  |
 *                      : +-------------------------------------+   +--------------------------+
 *                      : | Propriétaire                        |   | Usager                   |
 *                      : +-------------------------------------+   +--------------------------+
 *                      : | Propriétaire : atom                 |   | Usager : atom            |
 *                      : | Places : natural > 1                |   +--------------------------+   
 *                      : | Lieu activité := last(Étapes).Étape | 
 *                      : | Heure de départ : heure := ...      |
 *                      : | Minute de départ : minute := ...    |
 *                      : +-------------------------------------+
 *                      :          / \
 *                      :          \ /
 * Étape <= Destination :           |
 *                      :           |
 *                      :           | 1, *
 *               +-----------------------------------+
 *               | Étapes                            |
 *               +-----------------------------------+
 *               | Propriétaire, Numéro d'étape      |
 *               | Propriétaire, Étape               |
 *               +-----------------------------------+
 *               | Propriétaire : atom               |
 *               | Numéro d'étape : natural          |
 *               | Étape : atom                      |
 *               | Durée : natural > 0               |
 *               | Heure de passage: heure := ...    |
 *               | Minute de passage : minute := ... |
 *               +-----------------------------------+
 * ==
 *
 * Une version équivalente sous la forme d'une donnée complexe est donnée avec le schéma à la UML suivant.
 *
 * ==
 * +---------------------------------+
 * | Tronçons                        |
 * +---------------------------------+
 * | Origine, Destination            |
 * +---------------------------------+
 * | Origine : atom                  |
 * | Destination : atom              |
 * | Durée piétonnière : natural > 0 |
 * +---------------------------------+
 *
 * +------------------------------------------------+   +----------------------------------------------+
 * | Propriétaire                                   |   | Usager                                       |
 * +------------------------------------------------+   +----------------------------------------------+
 * | Propriétaire : atom                            |   | Usager : atom                                |
 * | Domicile : noeud                               |   | Domicile : noeud                             |
 * | Places : natural > 0                           |   | Lieu activité : noeud                        |
 * | Heure maximale : {0, ..., 23} x {0, ..., 59}   |   | Heure maximale : {0, ..., 23} x {0, ..., 59} |
 * | Étapes : [(étape : atom, durée : natural > 0)] |   +----------------------------------------------+
 * +------------------------------------------------+
 * ==
 *
 * Il est possible de passer de l'un à l'autre.
 * C'est ce qui est fait, en générant des instances de problèmes aléatoires dans le format complexe.
 * Les prédicats relationnels ne sont qu'une réécriture de la version complexe.
 * Les deux formes peuvent être employées simultanément en fonction des besoins.
 *
 * Un certain nombre de contraintes plus complexes que celles portées sur les schémas sont décrites par les post-conditions des prédicats.
 *
 * __Attention.__  Il existe quelques différences entres les prédicats et les schémas.
 * Il s'agit d'ajout ou retrait de certaines composantes dérivables ou peu utiles.
 *
 * @author José Martinez, July 15th, 2019
 * @see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
 * @license proprietary
 *
 * This code cannot be reproduced, copied, transmitted, etc., without the previous
 * explicit authorisation of the author and/or the School.
 * It is provided only for a pedagogical usage.
 *
 * @version 1.0
 *
 * === ALWAYS DOCUMENT AND TEST YOUR CODE!
 *
 * Documentation and tests are always much longer than your actual code...
 * Never neglect the usefulness of the latter with an untyped (or dynamically typed) language, especially when changing some line of code here and there.
 *
 * To see the documentation of this module:
 * - start the interactive interpreter at the shell prompt:  "swipl";
 * - run the HTTP documentation server: "doc_server(4000).";
 * - load this module:  "consult('cobicyclage_analyse.pl').";
 * - start (a new tab on) on your Web browser (with javascript enabled):  "doc_browser.".
 * - "Et voilà!"
 *
 */

:- module(cobicyclage_analyse, [ troncon/3
                               , etape/5
                               , parcours_reel/5
                               , parcours_virtuel/4
                               , personne_mobile/4
                               , proprietaire/6
                               , usager/4
                               , duree/1
                               , horaire/1
                               ]).

:- use_module(utilitaires, [ all_different/1 ]).
:- use_module(temps, [ duree/1
                     , horaire/1
                     , difference_horaire/3
                     , horaire_inferieur/2
                     ]).

%! troncon(?O:  atom,  ?D:  atom,  ?T:  natural) is nondet.
% 
% L'ensemble des tronçons constitue la carte (ou graphe) sur laquelle évolue les acteurs.
%
% Cela constitue l'un des trois principaux prédicats de description du problème, dans sa forme dite "complexe".
%
% @arg O       Noeud d'origine d'un tronçon.
% @arg D       Noeud de destination du tronçon.
% @arg T       Durée du déplacement entre l'origine et la destination.
%
% @throws Postcondition.   Le noeud d'origine est différent de celui de destination.
% @throws Postcondition.   La durée du déplacement est un entier strictement positif.
%

%! parcours_reel(?P:  atom,  ?D:  domicile,  ?C:  natural,  ?H_max:  horaire,  ?ES:  list(etape(E:  noeud, T:  duree))) is nondet.
% 
% Un parcours réel décrit précisément les étapes du parcours de chaque propriétaire entre son domicile et son lieu d'activité.
% Un parcours réel débute au domicile puis enchaîne un certain nombre, non nul, d'étapes, de durées non nulles, la dernière étape étant implicitement le lieu d'activité.
%
% Cela constitue l'un des trois principaux prédicats de description du problème, dans sa forme dite "complexe".
%
% @arg P       Identifiant d'un propriétaire de vélo.
% @arg D       Domicile d'une personne mobile, propriétaire ou usager.
% @arg C       Nombre de places du vélo du propriétaire.
% @arg H_max   Heure maximale d'arrivée d'un propriétaire sur son lieu d'activité.
% @arg ES      Étapes du parcours réel d'un propriétaire.
%
% @throws Postcondition.   Il y a au moins un propriétaire.
% @throws Postcondition.   L'identifiant d'une personne mobile est une clé de la "relation".
% @throws Postcondition.   Il y a au moins une étape.
% @throws Postcondition.   Toutes les étapes sont distinctes sur un parcours.
% @throws Postcondition.   Toutes les durées sont strictement positives.
% @throws Postcondition.   Les propriétaires sont distincts des usagers.
%

%! parcours_virtuel(?S:  atom,  ?D:  domicile,  ?A:  activite,  ?H_max:  horaire) is nondet.
% 
% Un parcours virtuel décrit le besoin de déplacement d'un usager sans vélo.
%
% Cela constitue l'un des trois principaux prédicats de description du problème, dans sa forme dite "complexe".
%
% @arg S       Identifiant d'un usager
% @arg D       Domicile d'une personne mobile, propriétaire ou usager.
% @arg A       Lieu d'activité d'un usager.
% @arg H_max   Heure maximale d'arrivée d'un usager sur son lieu d'activité.
%
% @throws Postcondition.   Il y a au moins un usager.
% @throws Postcondition.   L'identifiant d'une personne mobile est une clé de la "relation".
% @throws Postcondition.   Toutes les étapes sont distinctes sur un parcours (seulement le départ et l'arrivée pour un usager).
%

% Les données doivent être fournies en extension dans ce fichier.
:- include('cobicyclage_instance.pl').

:- begin_tests(troncons).

test(typage_troncon) :-
   assertion( forall( troncon(O, D, T)
                    , ( noeud(O)
                      , noeud(D)
                      , duree(T)
                      )
                    ) ).

test(origine_destination_differents) :-
   assertion( forall( troncon(O, D, _)
                    , O \= D
                    ) ).

test(duree_strictement_positive) :-
   assertion( forall( troncon(_, _, T)
                    , T > 0
                    ) ).

:- end_tests(troncons).

:- begin_tests(parcours_reels).

test(typage_parcours_reel) :-
   assertion( forall( parcours_reel(P, D, C, H_max, ES) % pour tous les parcours réels
                    , ( atom(P)                         % le propriétaire
                      , atom(D)                         % et son domicile doivent être des atomes,
                      , integer(C)                      % la capacité de son vélo est un entier
                      , C > 1                           % naturel au moins égal à deux
                      , horaire(H_max)                  
                      , forall( member(etape(E, T), ES)
                              , ( atom(E)               % toutes les étapes de son parcours sont des atomes
                                , duree(T)              % et chaque temps de parcours une durée valide
                                , T > 0                 % et non nulle
                                )
                              )
                      )
                    ) ).

test(au_moins_un_proprietaire) :-
   findall(P, parcours_reel(P, _, _, _, _), PS),
   length(PS, N_PS),
   assertion( N_PS >= 1 ).

test(proprietaire_cle) :-
   findall(P, parcours_reel(P, _, _, _, _), PS),
   assertion( all_different(PS) ).

test(au_moins_une_etape) :-
   assertion( forall( parcours_reel(_, _, _, _, ES) % pour tous les propriétaires
                    , ES \= []                      % il y a au moins une étape dans leur parcours (le lieu d'activité en fin)
                    ) ).

test(extremites_etapes_distinctes_proprietaires) :-
   assertion( forall( parcours_reel(_, D, _, _, ES)             % pour tous les propriétaires
                    , ( findall(E, member(etape(E, _), ES), NS) % leurs étapes
                      , all_different([D | NS])                 % doivent être différentes les unes des autres (incluant le lieu d'activité), et du domicile
                      )
                    ) ).

:- end_tests(parcours_reels).

:- begin_tests(parcours_virtuels).

test(typage_parcours_virtuel) :-
   assertion( forall( parcours_virtuel(S, D, A, H_max) % pour tous les parcours virtuels
                    , ( atom(S)                        % l'usager,
                      , atom(D)                        % son domicile
                      , atom(A)                        % et son lieu d'activité doivent être des atomes
                      , horaire(H_max)                 % et l'heure maximale d'arrivée un horaire valable
                      )
                    ) ).

test(au_moins_un_usager) :-
   findall(S, parcours_virtuel(S, _, _, _), SS),
   length(SS, N_SS),
   assertion( N_SS >= 1 ).

test(usager_cle) :-
   findall(S, parcours_virtuel(S, _, _, _), SS),
   assertion( all_different(SS) ).

test(extremites_distinctes_usagers) :-
   assertion( forall( parcours_virtuel(_, D, A, _)
                    , D \= A
                    ) ).

:- end_tests(parcours_virtuels).

:- begin_tests(parcours_reels_virtuels).

test(proprietaires_usagers_distincts) :-
   assertion( forall( parcours_reel(P, _, _, _, _)
                    , \+ parcours_virtuel(P, _, _, _)
                    ) ),
   assertion( forall( parcours_virtuel(S, _, _, _)
                    , \+ parcours_reel(S, _, _, _, _)
                    ) ).

:- end_tests(parcours_reels_virtuels).

%! noeud(?N:  noeud) is nondet.
% 
% Ensemble des noeuds constituant la carte ou graphe des déplacements.
%
% @arg N  Identifiant d'un noeud du graphe
%
% @throws Postcondition.  Tous les noeuds apparaissent dans au moins un tronçon, comme origine ou destination.
%
noeud(N) :-
   parcours_reel(_, N, _, _, _).   % le domicile d'un propriétaire est un noeud
noeud(N) :-
   parcours_reel(_, _, _, _, ES),
   member(etape(N, _), ES).        % chaque étape d'un parcours d'un propriétaire, incluant le lieu d'activité en fin, est un noeud
noeud(N) :-
   parcours_virtuel(_, N, _, _).   % le domicile d'un usager est un noeud
noeud(N) :-
   parcours_virtuel(_, _, N, _).   % le lieu d'activité d'un usager est un noeud

:- begin_tests(noeuds).

test(noeuds_dans_troncons) :-
   assertion( forall( noeud(N)
                    , ( troncon(N, _, _)
                      ; troncon(_, N, _)
                      )
                    ) ).

:- end_tests(noeuds).

%! domicile(?D:  noeud) is nondet.
% 
% Sous-ensemble des noeuds, 'noeuds/1', qui sont associés au domicile d'un propriétaire ou d'un usager.
%
% @arg D  Identifiant d'un domicile.
%
% @throws Postcondition.   Tous les domiciles sont des noeuds.
% @throws Postcondition.   Un domicile est un noeud associé explicitement au domicile d'un propriétaire ou d'un usager.
%
domicile(D) :-
   parcours_reel(_, D, _, _, _).  % le domicile d'un propriétaire...
domicile(D) :-
   parcours_virtuel(_, D, _, _).  % ... ou d'un usager est un... domicile !

:- begin_tests(domiciles).

test(sont_des_noeuds) :-
   assertion( forall( domicile(D)
                    , noeud(D)
                    ) ).

test(sont_associes) :-
   assertion( forall( domicile(D)
                    , ( proprietaire(_, D, _, _, _, _)
                      ; usager(_, D, _, _)
                      )
                    ) ).

:- end_tests(domiciles).

%! proprietaire(?P:  atom,  ?D:  noeud,  ?A:  noeud,  ?C:  natural,  ?H_max:  horaire,  ?H_min:  horaire) is nondet.
% 
% Sous-ensemble des personnes mobiles qui sont propriétaires d'un vélo avec quelques informations associées.
%
% @arg P       Identifiant d'un propriétaire.
% @arg D       Identifiant de son lieu de domicile.
% @arg A       Identifiant de son lieu d'activité.
% @arg C       Nombre de places sur le vélo du propriétaire.
% @arg H_max   Horaire maximal d'arrivée sur le lieu d'activité.
% @arg H_min   Horaire de départ compte tenu des durées et de l'horaire maximal.
%
% @throws Postcondition.   Il y a au moins un propriétaire, sinon pas de solution possible autre que de tout parcourir à pieds
% @throws Postcondition.   L'identifiant du propriétaire est une clé de la relation.
% @throws Postcondition.   Une personne mobile est un propriétaire si, et seulement si, il lui est associé un parcours réel.
% @throws Postcondition.   De manière complémentaire, une personne mobile est un usager si, et seulement si, il lui est associé un parcours virtuel.
% @throws Postcondition.   Le nombre de places est un entier strictement supérieur à l'unité.
% @throws Postcondition.   L'heure de départ est strictement inférieure à l'heure maximale d'arrivée.
%
proprietaire(P, D, A, C, H_max, H_min) :-
   parcours_reel(P, D, C, H_max, ES),    % la plupart des informations associées sont fournies en début du parcours réel
   last(ES, etape(A, _)),                % le lieu d'activité du propriétaire étant implicitement la dernière étape de son parcours
   findall( T
          , member(etape(_, T), ES)
          , TS
          ),
   sum_list(TS, T),                      % la durée totale du parcours
   difference_horaire(H_min, H_max, T).  % est soustraite de l'heure maximale d'arrivée afin de déterminer l'heure de départ.

:- begin_tests(proprietaires).

test(typage_proprietaire) :-
   assertion( forall( proprietaire(P, D, A, C, H_max, H_min)
                    , ( atom(P)
                      , noeud(D)
                      , noeud(A)
                      , integer(C)
                      , horaire(H_max)
                      , horaire(H_min)
                      )
                    ) ).

test(au_moins_un_proprietaire) :-
   findall(P, proprietaire(P, _, _, _, _, _), PS),
   assertion( PS \= [] ).

test(proprietaire_cle) :-
   findall(P, proprietaire(P, _, _, _, _, _), PS),
   assertion( all_different(PS) ).

test(typage_parcours_reel) :-
   assertion( forall( parcours_reel(P, D, C, H_max, ES)  % pour tous les parcours réels
                    , ( atom(P)                          % le propriétaire
                      , atom(D)                          % et son domicile doivent être des atomes,
                      , integer(C)                       % le nombre de places, un entier
                      , horaire(H_max)
                      , forall( member(etape(E, T), ES)
                              , ( atom(E)                % toutes les étapes de son parcours, des atomes
                                , duree(T)               % et le temps de parcours une durée valide
                                )
                              )
                      )
                    ) ).

test(places_strictement_superieure_unite) :-
   assertion( forall( proprietaire(_, _, _, C, _, _)
                    , C > 1
                    ) ).

test(heure_depart_strictement_inferieure_heure_maximale_arrivee) :-
   assertion( forall( proprietaire(_, _, _, _, H_max, H_min)
                    , horaire_inferieur(H_min, H_max)
                    ) ).

:- end_tests(proprietaires).

%! usager(?S:  atom,  ?D:  noeud,  ?A:  noeud,  ?H_max:  heure) is nondet.
% 
% Sous-ensemble des personnes mobiles qui ne sont _pas_ propriétaires d'un vélo avec quelques informations associées.
%
% @arg S       Identifiant d'un usager.
% @arg D       Identifiant de son lieu de domicile.
% @arg A       Identifiant de son lieu d'activité.
% @arg H_max   Heure maximale d'arrivée quotidienne sur son lieu d'activité.
%
% @throws Postcondition.   Il y a au moins un usager, sinon pas de problème !
% @throws Postcondition.   L'identifiant de l'usager est une clé de la relation.
% @throws Postcondition.   Le domicile est différent du lieu d'activité.
%
usager(S, D, A, H_max) :-
   parcours_virtuel(S, D, A, H_max).

:- begin_tests(usagers).

test(typage_usager) :-
   assertion( forall( usager(S, D, A, H_max)
                    , ( atom(S)
                      , noeud(D)
                      , noeud(A)
                      , horaire(H_max)
                      )
                    ) ).

test(au_moins_un_usager) :-
   findall(S, usager(S, _, _, _), SS),
   assertion( SS \= [] ).

test(usager_cle) :-
   findall(S, usager(S, _, _, _), SS),
   assertion( all_different(SS) ).

test(usager_lieux_differents) :-
   assertion( forall( usager(_, D, A, _)
                    , D \= A
                    ) ).

:- end_tests(usagers).

%! personne_mobile(?M:  atom,  ?D:  atom,  ?A:  atom,  ?H_max:  horaire) is nondet.
% 
% L'ensemble des personnes concernées par des problèmes de mobilité.
%
% @arg M        Identifiant d'une personne mobile.
% @arg D        Domicile de la personne.
% @arg A        Lieu d'activité de la personne.
% @arg H_max    Heure d'arrivée au plus tard sur le lieu d'activité de la personne.
%
% @throws Postcondition.   Les personnes mobiles sont tous les propriétaires et usagers, et seulement eux.
% @throws Postcondition.   Chaque personne joue un et un seul rôle.
%
personne_mobile(P, D, A, H_max) :-
   proprietaire(P, D, A, _, H_max, _).
personne_mobile(S, D, A, H_max) :-
   usager(S, D, A, H_max).

:- begin_tests(personnes_mobiles).

test(union_couvrante) :-
   assertion( ( forall( proprietaire(P, _, _, _, _, _)  % tout propriétaire est
                      , personne_mobile(P, _, _, _)     % une personne mobile
                      )
              , forall( usager(S, _, _, _)              % et tout usager est
                      , personne_mobile(S, _, _, _)     % une personne mobile
                      )
              ) ).

test(disjonction) :-
   assertion( forall( personne_mobile(M, _, _, _)          % toute personne mobile est
                    , \+ ( proprietaire(M, _, _, _, _, _)  % soit un propriétaire
                         , usager(M, _, _, _)              % soit un usager (pas les deux à la fois)
                         )
                    ) ).

:- end_tests(personnes_mobiles).

%! etape(?P:  proprietaire, ?O:  noeud,  ?D:  noeud, ?T:  duree,  ?H:  horaire) is nondet.
% 
% Ensemble des étapes qu'un propriétaire doit parcourir entre son domicile et son lieu d'activité, incluant les temps de transport à vide.
%
% @arg P  Identifiant d'un propriétaire.
% @arg O  Une étape d'origine sur le parcours.
% @arg D  Son étape de destination sur le parcours.
% @arg T  Temps moyen sur cette étape par ce propriétaire, généralement inférieure à très inférieure au temps mis par un piéton.
% @arg T  Heure de passage à cette étape.
%
% @throws Postcondition.   Pour chaque propriétaire, il y a au moins une étape, sinon il est en télétravail et ne participe donc ni au problème ni encore moins à sa solution.
% @throws Postcondition.   La durée d'une étape est strictement positive.
% @throws Postcondition.   L'heure d'arrivée de la dernière étape est égale à l'heure maximale d'arrivée du propriétaire.
%
etape(P, D, E, T_E, H) :-                           % première étape, depuis le domicile
   parcours_reel(P, D, _, _, [etape(E, T_E) | _]),
   proprietaire(P, _, _, _, _, H_min),
   difference_horaire(H_min, H, T_E).
etape(P, O, D, T_D, H) :-                           % étapes suivantes
   parcours_reel(P, _, _, _, ES),
   proprietaire(P, _, _, _, _, H_min),
   append(ES_pre, [etape(O, T_O), etape(D, T_D) | _], ES),
   findall( T
          , member(etape(_, T), [etape(O, T_O), etape(D, T_D) | ES_pre])
          , TS
          ),
   sumlist(TS, TS_sum),
   difference_horaire(H_min, H, TS_sum).

:- begin_tests(etapes).

test(typage_etapes) :-
   assertion( forall( etape(P, O, D, T, H)
                    , ( proprietaire(P, _, _, _, _, _)
                      , noeud(O)
                      , noeud(D)
                      , duree(T)
                      , horaire(H)
                      )
                    ) ).


test(non_teletravailleur) :-
   assertion( forall( proprietaire(P, _, _, _, _, _)
                    , etape(P, _, _, _, _)
                    ) ).

test(duree_strictement_positive) :-
   assertion( forall( etape(_, _, _, T, _)
                    , T > 0
                    ) ).

test(heure_arrivee_derniere_etape_correcte) :-
   assertion( forall( proprietaire(P, _, A, _, H_max, _)
                    , etape(P, _, A, _, H_max)
                    ) ).

:- end_tests(etapes).

:- run_tests([ troncons
             , parcours_reels
             , parcours_virtuels
             , parcours_reels_virtuels
             , noeuds
             , domiciles
             , proprietaires
             , usagers
             , personnes_mobiles
             , etapes
             ]).

