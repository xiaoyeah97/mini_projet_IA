% gvim:  fileencoding=utf8

/** <module> Génération aléatoire d'instances du problème du co-bicyclage
 *
 * @author José Martinez, July 16th, 2019
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
 * - load this module:  "consult('probleme.pl').";
 * - start (a new tab on) on your Web browser (with javascript enabled):  "doc_browser.".
 * - "Et voilà!"
 *
 * La génération aléatoire produit une instance basée sur le modèle complexe dont le schéma est rappelé ci-dessous.
 * Il y manque la définition du lieu d'activité de manière calculée et la relation / classe des personnes mobiles qui n'est que l'union sur les attributs communs des propriétaires et usagers.
 * Notez que pour distinguer ces prédicat de ceux utilisés par le modèle relationnel, les propriétaires sont traduits par 'parcours_reel/5' et les usagers par 'parcours_virtuel/4'.
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
 * | Places : natural > 1                           |   | Lieu activité : noeud                        |
 * | Étapes : [(étape : atom, durée : natural > 0)] |   | Heure maximale : {0, ..., 23} x {0, ..., 59} |
 * +------------------------------------------------+   +----------------------------------------------+
 * ==
 *
 */

:- module(cobicyclage_random, [ random_extension_noeuds/1
                              , random_parcours_virtuel/3
                              , random_usagers/3
                              , random_etapes/3
                              , random_parcours_reel/3
                              , random_proprietaires/3
                              , random_parcours/1
                              , random_troncons/2
                              , random_instance/1
                              , parcours_prolog/3
                              , parcours_graphviz/3
                              ]).

:- use_module(utilitaires, [ all_different/1
                           , unique/2
                           ]).

:- use_module(temps, [ duree/1
                     , horaire/1
                     , horaire_between/3
                     , random_duree_between/3
                     , random_horaire/1
                     , random_horaire_between/3
                     ]).

%! intervalle_nombre_proprietaires(?P_min:  natural,  ?P_max:  natural) is det.
%! intervalle_nombre_usagers(?S_min:  natural,  ?S_max:  natural) is det.
%! intervalle_nombre_noeuds(?N_min:  natural,  ?N_max:  natural) is det.
%! intervalle_duree(?T_min:  natural,  ?T_max:  natural) is det.
%! intervalle_nombre_etapes(?E_min:  natural,  ?E_max:  natural) is det.
%! intervalle_horaire(?H_min:  horaire,  ?H_max:  horaire) is det.
%
% Intervalles de variations de différents paramètres contrôlant la complexité d'instances à générer aléatoirement.
%
% @throws Postcondition.   Les valeurs minimales doivent être inférieures ou égales aux valeurs maximales correspondantes.
% @throws Postcondition.   Le nombre d'étapes maximales pour le parcours d'un propriétaire doit être inférieur ou égal au nombre minimal de noeuds.
% @throws Postcondition.   La durée minimale est supérieure ou égale à l'unité.
% @throws Postcondition.   Le nombre minimal de place est au moins l'unité.
%
% @tbd   La deuxième post-condition pourrait être assouplie, voire supprimée, en utilisant comme, limite véritable, le nombre de noeuds sélectionnés aléatoirement au moment d'une génération.
%
intervalle_nombre_proprietaires(3, 6).           % intervalle_nombre_proprietaires(3, 6).
intervalle_nombre_usagers(5, 8).                 % intervalle_nombre_usagers(5, 8).
intervalle_nombre_noeuds(10, 20).                % intervalle_nombre_noeuds(10, 20).
intervalle_duree(2, 8).                          % intervalle_duree(2, 8).
intervalle_nombre_etapes(2, 5).                  % intervalle_nombre_etapes(3, 5).
intervalle_nombre_places(2, 6).                  % intervalle_nombre_places(2, 4).
intervalle_horaire(heure(8, 45), heure(9, 15)).  % intervalle_horaire(heure(6, 45), heure(9, 30)).

:- begin_tests(contraintes_intervalles).

test(intervalles_non_vides) :-
   intervalle_nombre_proprietaires(P_min, P_max),
   assertion( P_min =< P_max ),
   intervalle_nombre_usagers(S_min, S_max),
   assertion( S_min =< S_max ),
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( N_min =< N_max ),
   intervalle_duree(T_min, T_max),
   assertion( T_min =< T_max ),
   intervalle_nombre_etapes(E_min, E_max),
   assertion( E_min =< E_max ).

test(nombre_etapes_maximal_inferieur_ou_egal_nombre_noeuds_minimal) :-
   intervalle_nombre_etapes(_, E_max),
   intervalle_nombre_noeuds(N_min, _),
   assertion( E_max =< N_min ).

test(duree_minimale_au_moins_un) :-
   intervalle_duree(T_min, _),
   assertion( T_min >= 1 ).

test(places_minimales_au_moins_un) :-
   intervalle_nombre_places(C_min, _),
   assertion( C_min >= 1 ).

:- end_tests(contraintes_intervalles).

%! nom_atomique(+P:  atom,  +N:  integer,  -A:  atom) is det.
%
% Création d'un identifiant atomique à partir d'un racine et d'un numéro d'ordre.
%
% @arg P   Préfixe atomique.
% @arg N   Numéro.
% @arg A   Atome consistant en la concaténation du préfixe avec le numéro fourni.
%
% @bug  Ce prédicat devrait se trouver dans 'utilitaires.pl'.
%
nom_atomique(P, N, A) :-
   atom_number(A_N, N),
   atom_concat(P, A_N, A).

%! extension_noeuds(+N:  natural, -NS:  list(atom)) is det.
%
% Génération en extension de l'ensemble des noeuds d'une instance.
%
% @arg N    Le nombre de noeuds à générer.
% @arg NS   La liste des différents atomes servant de noms aux noeuds.
%
% @throws Precondition.   Le nombre de noeuds à générer est un entier naturel.
%
% @throws Postcondition.   La liste de noms contient le nombre requis de noeuds.
% @throws Postcondition.   Tous les noms sont différents les uns des autres.
%
extension_noeuds(N, NS) :-
   assertion( N >= 0 ),
   findall( NI
          , ( between(1, N, I)
            , nom_atomique('nd_', I, NI)
            )
          , NS
          ).

:- begin_tests(extension_noeuds).

test(nombre_noeuds_demande) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , length(NS, N)
                      )
                    ) ).

test(tous_differents) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , all_different(NS)
                      )
                    ) ).

:- end_tests(extension_noeuds).

%! random_extension_noeuds(-NS:  list(atom)) is det.
%
% Génération d'une extension de noeuds, de taille aléatoire mais contrôlée par 'intervalle_nombre_noeuds/2'.
%
% @arg NS   La liste des différents atomes servant de noms aux noeuds.
%
% @throws Postcondition.   La liste de noms contient un nombre de noeuds compris dans l'intervalle correspondant.
% @throws Postcondition.   Tous les noms sont différents les uns des autres.
%
random_extension_noeuds(NS) :-
   intervalle_nombre_noeuds(N_min, N_max),
   random_between(N_min, N_max, N),
   extension_noeuds(N, NS).

:- begin_tests(random_extension_noeuds).

test(nombre_noeuds_autorisé) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_extension_noeuds(NS)
                      , length(NS, N_NS)
                      , between(N_min, N_max, N_NS)
                      )
                    ) ).

test(tous_differents) :-
   assertion( forall( between(1, 100, _)
                    , ( random_extension_noeuds(NS)
                      , all_different(NS)
                      )
                    ) ).

:- end_tests(random_extension_noeuds).

%! random_horaire_embauche(?H:  horaire) is det.
%
% @arg H   Horaire constitué d'une heure et de minutes, choisi aléatoirement dans l'intervalle contrôlé par 'intervalle_horaire/2'.
%
% @throws Postcondition.   L'horaire est bien un horaire situé dans l'intervalle horaire autorisé pour embaucher.
%
random_horaire_embauche(H) :-
   intervalle_horaire(H_min, H_max),
   random_horaire_between(H_min, H_max, H).

:- begin_tests(random_horaires_embauche).

test(dans_intervalle_implicite) :-
   intervalle_horaire(H_min, H_max),
   assertion( forall( between(1, 100, _)
                    , ( random_horaire_embauche(H)
                      , horaire_between(H_min, H_max, H)
                      )
                    ) ).

:- end_tests(random_horaires_embauche).

%! random_parcours_virtuel(+S:  atom, +NS:  list(noeud),  -P:  parcours_virtuel) is det.
%
% Génération d'un parcours virtuel aléatoire pour l'usager indiqué, domicile et lieu d'activité devant être distincts.
%
% @arg S    Identifiant de l'usager.
% @arg NS   La liste des noeuds du graphe.
% @arg PS   Le parcours virtuel associé aléatoirement à l'usager.
%
% @throws Precondition.   Le nombre de noeuds doit être au moins égal à deux (domicile et lieu d'activité).
% @throws Precondition.   Tous les noeuds sont deux à deux différents.
%
% @throws Postcondition.   Le domicile et le lieu d'activité du parcours sont différents.
% @throws Postcondition.   L'usager du parcours virtuel est celui indiqué.
%
random_parcours_virtuel(S, NS, parcours_virtuel(S, D, A, H_max)) :-
   assertion( ( length(NS, N_NS)
              , N_NS >= 2
              ) ),
   assertion( all_different(NS) ),
   random_member(D, NS),            % choix aléatoire du domicile de l'usager parmi les noeuds
   delete(NS, D, NS_moins_D),
   random_member(A, NS_moins_D),    % choix aléatoire du lieu d'activité de l'usager à l'exclusion du domicile
   random_horaire_embauche(H_max).  % choix aléatoire de l'heure maximale d'arrivée

:- begin_tests(random_parcours_virtuel).

test(domicile_different_lieu_activite) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_parcours_virtuel(_, NS, parcours_virtuel(_, D, A, _))
                      , D \= A
                      )
                    ) ).

test(usager_indique) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_parcours_virtuel(usager_lambda, NS, parcours_virtuel(usager_lambda, _, _, _))
                      )
                    ) ).

:- end_tests(random_parcours_virtuel).

%! random_usagers(+N:  natural,  NS:  list(noeud),  PS:  list(parcours_virtuel)) is det.
%
% Génération d'un nombre indiqué d'usagers et parcours virtuels associés.
%
% @arg N    Le nombre d'usagers à générer.
% @arg NS   La liste des noeuds du graphe.
% @arg PS   Les parcours virtuels des usagers.
%
% @throws Precondition.   Le nombre de noeuds doit être au moins égal à deux (domicile et lieu d'activité).
% @throws Precondition.   Tous les noeuds sont deux à deux différents.
%
% @throws Postcondition.   Le nombre d'usagers demandé a été généré.
% @throws Postcondition.   Les usagers sont différents les uns des autres.
% @throws Postcondition.   Le domicile et le lieu d'activité sont différents.
%
random_usagers(N, NS, PS) :-
   assertion( ( length(NS, N_NS)
              , N_NS >= 2
              ) ),
   assertion( all_different(NS) ),
   findall( P
          , ( between(1, N, I)
            , nom_atomique('usr_', I, S_I)        % création du i^e usager sous l'identifiant atomique 'usr_i'
            , random_parcours_virtuel(S_I, NS, P) % et association d'un parcours virtuel aléatoire
            )
          , PS
          ).

:- begin_tests(random_usagers).

test(nombre_usagers_demande) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_usagers(S_min, S_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N_N)
                      , extension_noeuds(N_N, NS)
                      , random_between(S_min, S_max, N_S)
                      , random_usagers(N_S, NS, PS)
                      , length(PS, N_S)
                      )
                    ) ).

test(usagers_distincts) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_usagers(S_min, S_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N_N)
                      , extension_noeuds(N_N, NS)
                      , random_between(S_min, S_max, N_S)
                      , random_usagers(N_S, NS, PS)
                      , findall( S
                               , member(parcours_virtuel(S, _, _, _), PS)
                               , SS
                               )
                      , all_different(SS)
                      )
                    ) ).

test(extremites_parcours_distinctes) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_usagers(S_min, S_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N_N)
                      , extension_noeuds(N_N, NS)
                      , random_between(S_min, S_max, N_S)
                      , random_usagers(N_S, NS, PS)
                      , forall( member(parcours_virtuel(_, D, A, _), PS)
                              , D \= A
                              )
                      )
                    ) ).

:- end_tests(random_usagers).

%! random_duree(+T:  duree) is det.
%
% @arg T   Une durée aléatoire comprise dans l'intervalle contrôlé par 'intervalle_duree/2'.
%
% @throws Postcondition.   La durée est bien dans l'intervalle de contrôle.
%
random_duree(T) :-
   intervalle_duree(T_min, T_max),
   random_duree_between(T_min, T_max, T).

:- begin_tests(random_durees).

test(dans_intervalle_implicite) :-
   intervalle_duree(T_min, T_max),
   assertion( forall( between(1, 100, _)
                    , ( random_duree(T)
                      , between(T_min, T_max, T)
                      )
                    ) ).

:- end_tests(random_durees).

%! random_etapes(+N:  natural, +NS:  list(noeud),  -ES:  list(etape)) is det.
%
% Génération d'une liste d'étapes aléatoires, de longueur donnée, et construite sur la liste de noeuds fournie, sans possibilité de répétition.
% Il s'agit d'une adaptation de 'random_subset/3'.
%
% @arg N    Le nombre d'étapes que doit contenir le parcours.
% @arg NS   La liste des noeuds utilisables pour construire le parcours.
% @arg ES   La liste des étapes construites aléatoirement.
%
% @throws Precondition.   Le nombre de noeuds est supérieur ou égal au nombre d'étapes demandé.
% @throws Precondition.   Les noeuds sont deux à deux distincts.
%
% @throws Postcondition.   La longueur du résultat est égale au nombre d'étapes demandé.
% @throws Postcondition.   Les noeuds des étapes sont deux à deux distincts.
% @throws Postcondition.   Les durée des étapes sont strictement positives.
%
random_etapes(0, _, []).
random_etapes(N, NS, [etape(E, T) | ES]) :-
   assertion( (length(NS, N_NS)
              , N_NS >= N
              ) ),
   random_member(E, NS),
   random_duree(T),
   plus(N_moins_1, 1, N),
   delete(NS, E, NS_moins_E),
   random_etapes(N_moins_1, NS_moins_E, ES).

:- begin_tests(random_etapes).

test(longueur_demandee) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_etapes(E_min, _),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_between(E_min, N, N_E)
                      , random_etapes(N_E, NS, ES)
                      , length(ES, N_E)
                      )
                    ) ).

test(noeuds_differents) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_etapes(E_min, _),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_between(E_min, N, N_E)
                      , random_etapes(N_E, NS, ES)
                      , findall( E
                               , member(etape(E, _), ES)
                               , NES
                               )
                      , all_different(NES)
                      )
                    ) ).

test(durees_strictement_positives) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_etapes(E_min, _),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_between(E_min, N, N_E)
                      , random_etapes(N_E, NS, ES)
                      , forall( member(etape(_, T), ES)
                              , T > 0
                              )
                      )
                    ) ).

:- end_tests(random_etapes).

%! random_parcours_reel(+N:  atom, +NS:  list(noeud),  -P:  parcours_reel) is det.
%
% Génération d'un parcours réel aléatoire associé au propriétaire indiqué.
%
% @arg N    Nom du propriétaire.
% @arg NS   La liste des noeuds du graphe.
% @arg PS   Le parcours réel associé aléatoirement à un propriété indiqué, de longueur aléatoire contrôlé par 'intervalle_nombre_etapes/2'.
%
% @throws Precondition.   Le nombre de noeuds doit être au moins égal à deux (domicile et lieu d'activité).
% @throws Precondition.   Tous les noeuds sont deux à deux différents.
%
% @throws Postcondition.   Le nom du propriétaire dans le parcours est celui fourni.
% @throws Postcondition.   Son domicile a été choisi dans la liste des noeuds.
% @throws Postcondition.   Le nombre de places de son vélo est compris dans l'intervalle autorisé par 'intervalle_nombre_places/2'.
%
random_parcours_reel(N, NS, parcours_reel(N, D, C, H_max, ES)) :-
   assertion( ( length(NS, N_NS)
              , N_NS >= 2
              ) ),
   assertion( all_different(NS) ),
   random_member(D, NS),                       % choix aléatoire du domicile du propriétaires parmi les noeuds
   intervalle_nombre_places(C_min, C_max),
   random_between(C_min, C_max, C),            % choix aléatoire du nombre de places de son (multi-)vélo
   random_horaire_embauche(H_max),             % choix aléatoire de l'heure maximale d'arrivée
   delete(NS, D, NS_moins_D),
   length(NS_moins_D, N_NS_moins_D),
   intervalle_nombre_etapes(E_min, E_max),
   min_list([E_max, N_NS_moins_D], Min_E_max),
   random_between(E_min, Min_E_max, N_E),
   random_etapes(N_E, NS_moins_D, ES).         % choix aléatoire des étapes de son parcours, hors domicile

:- begin_tests(random_parcours_reel).

test(nom_proprietaire_fourni) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_parcours_reel(proprietaire_lambda, NS, parcours_reel(proprietaire_lambda, _, _, _, _))
                      )
                    ) ).

test(domicile_appartient_noeuds) :-
   intervalle_nombre_noeuds(N_min, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_parcours_reel(proprietaire_lambda, NS, parcours_reel(_, D, _, _, _))
                      , member(D, NS)
                      )
                    ) ).

test(nombre_places_autorise) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_places(C_min, C_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N)
                      , extension_noeuds(N, NS)
                      , random_parcours_reel(proprietaire_lambda, NS, parcours_reel(_, _, C, _, _))
                      , between(C_min, C_max, C)
                      )
                    ) ).

:- end_tests(random_parcours_reel).

%! random_proprietaires(+N:  natural,  NS:  list(noeud),  PS:  list(parcours_reel)) is det.
%
% @arg N    Le nombre de propriétaires à générer.
% @arg NS   La liste des noeuds du graphe.
% @arg PS   Les parcours réels des propriétaires.
%
% @throws Precondition.   Le nombre de noeuds doit être au moins égal à deux (domicile et lieu d'activité).
% @throws Precondition.   Tous les noeuds sont deux à deux différents.
%
% @throws Postcondition.   Le nombre de propriétaires demandé a été généré.
% @throws Postcondition.   Le domicile et le lieu d'activité sont différents.
%
random_proprietaires(N, NS, PS) :-
   assertion( ( length(NS, N_NS)
              , N_NS >= 2
              ) ),
   assertion( all_different(NS) ),
   findall( P
          , ( between(1, N, I)
            , nom_atomique('prop_', I, N_I)     % création du i^e propriétaire sous l'identifiant atomique 'prop_i'
            , random_parcours_reel(N_I, NS, P)  % et association d'un parcours réel aléatoire
            )
          , PS
          ).

:- begin_tests(random_proprietaires).

test(nombre_proprietaires_demande) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_proprietaires(S_min, S_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N_N)
                      , extension_noeuds(N_N, NS)
                      , random_between(S_min, S_max, N_S)
                      , random_proprietaires(N_S, NS, PS)
                      , length(PS, N_S)
                      )
                    ) ).

test(extremites_parcours_distinctes) :-
   intervalle_nombre_noeuds(N_min, N_max),
   intervalle_nombre_proprietaires(S_min, S_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(N_min, N_max, N_N)
                      , extension_noeuds(N_N, NS)
                      , random_between(S_min, S_max, N_S)
                      , random_proprietaires(N_S, NS, PS)
                      , forall( member(parcours_virtuel(D, A, _), PS)
                              , D \= A
                              )
                      )
                    ) ).

:- end_tests(random_proprietaires).

%! random_parcours(-PS:  list(parcours_reel | parcours_virtuel)) is det.
%
% @arg PS   Une liste de parcours, aussi bien réels que virtuels.
%
% @throws Postcondition.   Il y a au moins un propriétaire.
% @throws Postcondition.   Il y a au moins un usager.
% @throws Postcondition.   Tous les propriétaires sont distincts.
% @throws Postcondition.   Tous les usagers sont distincts.
% @throws Postcondition.   Etc.
%
random_parcours(PS) :-
   random_extension_noeuds(NS),                   % création en extension d'un ensemble aléatoire de noms de noeuds du graphe
   intervalle_nombre_proprietaires(P_min, P_max),
   random_between(P_min, P_max, N_P),
   random_proprietaires(N_P, NS, PS_P),           % choix aléatoire du nombre de propriétaires génération d'autant de propriétaires que demandé
   intervalle_nombre_usagers(S_min, S_max),
   random_between(S_min, S_max, N_S),
   random_usagers(N_S, NS, PS_S),                 % idem pour les usagers
   append(PS_P, PS_S, PS).                        % leur concaténation fournissant l'ensemble des parcours de l'instance à résoudre

%! etape_parcours_reel(+PS:  parcours_reel,  ?P:  proprietaire, ?O:  noeud,  ?D:  noeud, ?T:  duree) is nondet.
%
% @arg PS   Un parcours réel.
% @arg P    Propriétaire.
% @arg O    Noeud d'origine d'une étape du parcours.
% @arg D    Noeud de destination de la même étape.
% @arg T    Durée de parcours de l'étape.
%
etape_parcours_reel(parcours_reel(P, D, _, _, [etape(E, T) | _]), P, D, E, T).  % première étape, depuis le domicile
etape_parcours_reel(parcours_reel(P, _, _, _, ES), P, O, D, T) :-               % étapes suivantes
   append(_, [etape(O, _), etape(D, T) | _], ES).

%! noeuds_un_parcours(+P:  parcours,  ?N:  noeud) is semidet.
%
% @arg P   Un parcours, réel ou virtuel.
% @arg N   Un noeud présent dans le parcours.
%
noeud_un_parcours(parcours_reel(_, D, _, _, _), D).     % le domicile d'un propriétaire est un noeud
noeud_un_parcours(parcours_reel(_, _, _, _, ES), E) :-  % les étapes sont des noeuds
   member(etape(E, _), ES).
noeud_un_parcours(parcours_virtuel(_, D, _, _), D).     % le domicile d'un usager est un noeud
noeud_un_parcours(parcours_virtuel(_, _, A, _), A).     % le lieu d'activité d'un usager est un noeud

%! noeud_parcours(+PS:  list(parcours),  ?N:  noeud) is semidet.
%
% @arg PS   Une liste de parcours, réels ou virtuels.
% @arg N    Un noeud présent dans l'un ou l'autre des parcours.
%
noeud_parcours(PS, N) :-
   member(P, PS),
   noeud_un_parcours(P, N).

%! noeuds_parcours(+PS:  list(parcours),  -NS:  list(noeud)) is semidet.
%
% "Récupération" des seuls noeuds utilisés dans les parcours.
%
% @arg PS   Une liste de parcours, réels ou virtuels.
% @arg NS   Les noeuds présents dans l'un ou l'autre des parcours, sans doublon.
%
noeuds_parcours(PS, NS) :-
   findall( N
          , noeud_parcours(PS, N)
          , NDS
          ),
   unique(NDS, NS).

%! troncons_inverses(+TS_dir:  list(tronçon),  -TS_inv:  list(tronçon)) is semidet.
%
% Inversion du sens de parcours des tronçons sans modification de la durée.
%
% @arg TS_dir   Une liste de tronçons.
% @arg TS_inv   La liste des tronçons inverses.
%
troncons_inverses(TS_dir, TS_inv) :-
   findall( troncon(D, O, TP)
          , member(troncon(O, D, TP), TS_dir)
          , TS_inv
          ).

%! troncons_parcours_reels(+PS:  list(parcours),  -TS:  list(troncon)) is semidet.
%
% "Récupération" des tronçons à partir des étapes des parcours.
%
% @arg PS   Une liste de parcours, réels ou virtuels.
% @arg TS   Les tronçons présents dans les parcours réels, à double sens et durée piétonnière au moins égale.
%
troncons_parcours_reels(PS, TS) :-
   findall( troncon(O, D, TP)
          , ( member(P, PS)
            , etape_parcours_reel(P, _, O, D, T)
            , random_between(2, 4, M)
            , TP is M * T             % multiplication par un coefficient aléatoire du temps piéton par rapport au temps à vélo.
            )
          , TS_dir
          ),  % récupération des tronçons existants pour les parcours réels avec des délais de parcours plus longs à pieds
   troncons_inverses(TS_dir, TS_inv),
   append(TS_dir, TS_inv, TS).

%! troncons_parcours_virtuels(+NS:  list(noeud),  +PS:  list(parcours),  -TS:  list(troncon)) is semidet.
%
% Construction de tronçons aléatoires sur les noeuds existants pour construire un parcours imaginaire sur un parcours virtuel !
%
% @arg NS   Les noeuds présents dans l'un ou l'autre des parcours, sans doublon.
% @arg PS   Une liste de parcours, réels ou virtuels.
% @arg TS   Les tronçons présents dans les parcours réels, à double sens et durée piétonnière au moins égale.
%
troncons_parcours_virtuels(NS, PS, TS) :-
   assertion( all_different(NS) ),
   findall( troncon(O, D, TP)
          , ( member(parcours_virtuel(_, Dom, A, _), PS)
            , delete(NS, Dom, NS_moins_Dom)
            , delete(NS_moins_Dom, A, NS_moins_Dom_moins_A)
            , random_parcours_reel(_, NS_moins_Dom_moins_A, P)  % construction d'un parcours imaginé pour un usager
            , ( etape_parcours_reel(P, _, O, D, T)              % avec les étapes imaginées
              ; P = parcours_reel(_, D, _, _, _),               % plus joindre le domicile à la première étape
                O = Dom,
                random_duree(T)
              ; P = parcours_reel(_, _, _, _, ES),              % et joindre la dernière étape au lieu d'activité
                last(ES, etape(O, _)),
                D = A,
                random_duree(T)
              )
            , random_between(1, 4, M)
            , TP is M * T             % multiplication par un coefficient aléatoire du temps piéton par rapport au temps à vélo.
            )
          , TS_dir
          ),
   troncons_inverses(TS_dir, TS_inv),
   append(TS_dir, TS_inv, TS).

%! troncons_uniques(+TS:  list(tronçon), -US:  list(tronçon)) is det.
%
% @arg TS   Une liste de tronçons avec d'éventuels doublons _sans tenir compte des durées_ et en intervertissant les deux noeuds.
% @arg US   La liste des tronçons sans doublons.
%
% @throws Postcondition.   Tous les éléments de US sont distincts les uns des autres.
%
troncons_uniques([],  []).
troncons_uniques([troncon(O, D, _) | TS], US) :-
   ( member(troncon(O, D, _), TS)
   ; member(troncon(D, O, _), TS)
   ),
   troncons_uniques(TS, US),
   !.
troncons_uniques([T | TS], [T | US]) :-
   troncons_uniques(TS, US).

%! troncons_symetriques(+TS:  list(tronçon), -SS:  list(tronçon)) is det.
%
% @arg TS   Une liste de tronçons.
% @arg SS   La liste des tronçons, doublés avec leurs symétriques.
%
troncons_symetriques([],  []).
troncons_symetriques([troncon(O, D, T) | TS], [troncon(O, D, T), troncon(D, O, T) | SS]) :-
   troncons_symetriques(TS, SS).

%! random_troncons(+PS:  list(parcours),  -TS:  list(tronçon)) is det.
%
% @arg PS   Une liste de parcours, aussi bien réels que virtuels.
% @arg TS   Une liste de tronçons décrivant le graphe.
%
% @throws Postcondition.   Toutes les étapes sont dans les tronçons.
%
random_troncons(PS, TS) :-
   troncons_parcours_reels(PS, TSP),
   noeuds_parcours(PS, NS),
   troncons_parcours_virtuels(NS, PS, TSS),
   append(TSP, TSS, TS_dup),
   troncons_uniques(TS_dup, TS_unq),
   troncons_symetriques(TS_unq, TS).

:- begin_tests(test_random_troncons).

test(etapes_dans_troncons) :-
   random_parcours(PS),
   random_troncons(PS, TS),
   assertion( forall( ( member(P, PS)
                      , etape_parcours_reel(P, _, O, D, _)
                      )
                    , member(troncon(O, D, _), TS)
                    ) ).

:- end_tests(test_random_troncons).

%! parcours_prolog(+N:  atom, +PS:  list(parcours),  +TS:  list(tronçon)) is det.
%
% @arg N    Nom du fichier dans lequel écrire.
% @arg PS   Une liste de parcours, aussi bien réels que virtuels.
% @arg PS   La liste des tronçons du graphe.
%
% @bug  Ce prédicat a comme effet de bord d'afficher les parcours sous la forme d'un prédicat Prolog en extension.
%
parcours_prolog(N, PS, TS) :-
   open(N, write, F),
   forall( member(T, TS)
         , ( write(F, T)
           , write(F, ".\n")
           )
         ),
   forall( ( member(P, PS)
           , P = parcours_reel(_, _, _, _, _)
           )
         , ( write(F, P)
           , write(F, ".\n")
           )
         ),
   forall( ( member(P, PS)
           , P = parcours_virtuel(_, _, _, _)
           )
         , ( write(F, P)
           , write(F, ".\n")
           )
         ),
   close(F).

%! couleur_parcours(?C:  atom) is nondet.
%
% @arg C   Une couleur X11 reconnue par Graphviz.
%
couleur_parcours(blue).
couleur_parcours(blueviolet).
couleur_parcours(deepskyblue).
couleur_parcours(darkgreen).
couleur_parcours(darkorange1).
couleur_parcours(deeppink).
couleur_parcours(gold).
couleur_parcours(indigo).
couleur_parcours(green).
couleur_parcours(greenyellow).
couleur_parcours(salmon).

%! random_couleur_parcours(?C:  atom) is det.
%
% @arg C   Une couleur X11 reconnue par Graphviz choisie aléatoirement parmi les couleurs disponibles dans 'couleur_parcours/1'.
%
random_couleur_parcours(C) :-
   findall(C, couleur_parcours(C), CS),
   length(CS, N_CS),
   random_between(1, N_CS, I),
   nth1(I, CS, C).

%! parcours_graphviz(+N:  atom,  +PS:  list(parcours),  +TS:  list(troncon)) is det.
%
% @arg N    Nom du fichier dans lequel écrire la description des parcours.
% @arg PS   Une liste de parcours, aussi bien réels que virtuels.
% @arg PS   La liste des tronçons du graphe.
%
% @bug  Ce prédicat a comme effet de bord d'afficher les tronçons et parcours sous la forme d'une description Graphviz.
%
parcours_graphviz(N, PS, TS) :-
   open(N, write, F),
   write(F, "graph\n"),
   write(F, "{\n"),
   write(F, "layout=dot;\n"),
   write(F, "margin=0;\n"),
   write(F, "rotate=90;\n"),
   write(F, "bgcolor=invis;\n"),
   write(F, "overlap=scale;\n"),
   write(F, "nodesep=0;\n"),
   write(F, "splines=true;\n"),
   write(F, "node [shape=ellipse];\n"),
   write(F, "edge [color=black, penwidth=2];\n"),
   forall( ( member(troncon(O, D, TP), TS)
           , compare(<, O, D)
           )  % pour éviter de dessiner les arcs (connus pour avoir été créés symétriques) dans les deux sens
         , format(F, "~w -- ~w [label=\"~w mn\", len=~w];\n", [O, D, TP, TP])
         ),
   forall( ( member(P, PS)
           , P = parcours_reel(Pr, _, C, heure(H, M), ES)
           )
         , ( random_couleur_parcours(L)
           , W is 2 * C
           , last(ES, etape(A, _))
           , format(F, "edge [color=~w, penwidth=~w];\n", [L, W])
           , forall( etape_parcours_reel(P, Pr, O, D, T)
                   , ( D \= A
                     -> format(F, "~w -- ~w [label=\"~w en ~w mn\", len=~w dir=forward, arrowhead=normal];\n", [O, D, Pr, T, T])
                     ;  format(F, "~w -- ~w [label=\"~w en ~w mn avant ~w h ~w mn\", len=~w, dir=forward, arrowhead=normal];\n", [O, D, Pr, T, H, M, T])
                     )
                   )
           )
         ),
   write(F, "edge [color=red, style=dashed, penwidth=2];\n"),
   forall( member(parcours_virtuel(S, D, A, heure(H_max, M_max)), PS)
         , format(F, "~w -- ~w [label=\"~w avant ~w h ~w\", dir=forward, arrowhead=normal];\n", [D, A, S, H_max, M_max])
         ),
   write(F, "}\n"),
   close(F).

%! random_instance(+N:  atom) is det.
%
% @arg N   Préfixe du nom du fichier devant recevoir une instance de problème aléatoire.
%
% Ce prédicat a comme effet de bord de créer (ou d'écraser) les fichiers dont le nom est fourni avec :
%
%    * une description sous la forme d'un prédicat Prolog en extension (suffixé .pl),
%
%    * une description sous la forme d'un fichier Graphviz (suffixé .dot).
%
% @arg N    Nom du propriétaire.
% @arg NS   La liste des noeuds du graphe.
% @arg PS   Le parcours réel associé aléatoirement à un propriété indiqué.
%
% @throws Precondition.   Le nombre de noeuds doit être au moins égal à deux (domicile et lieu d'activité).
% @throws Precondition.   Tous les noeuds sont deux à deux différents.
%
% @throws Postcondition.   Le nom du propriétaire dans le parcours est celui fourni, N.
% @throws Postcondition.   Son domicile a été choisi dans la liste des noeuds, NS.
% @throws Postcondition.   Le nombre de places de son vélo est compris dans l'intervalle autorisé par 'intervalle_nombre_places/2'.
%
% _Attention._  Les tests génèrent cent couples de fichiers ! 
% Ils sont dénommés "random_instance_i.pl" et "random_instance_i.dot" avec i variant de 1 à 100.
%
random_instance(N) :-
   random_parcours(PS),
   random_troncons(PS, TS),
   atom_concat(N, '.pl', N_pl),
   parcours_prolog(N_pl, PS, TS),
   atom_concat(N, '.dot', N_dot),
   parcours_graphviz(N_dot, PS, TS).

:- begin_tests(random_instances).

test(exemples) :-
   forall( between(1, 100, I)
         , ( nom_atomique('random_instance_', I, N_I)
           , random_instance(N_I)
           )
         ).

:- end_tests(random_instances).

:- run_tests([ contraintes_intervalles
             , extension_noeuds
             , random_extension_noeuds
             , random_horaires_embauche
             , random_parcours_virtuel
             , random_usagers
             , random_durees
             , random_etapes
             , random_parcours_reel
             , random_proprietaires
             , test_random_troncons
             , random_instances
             ]).

