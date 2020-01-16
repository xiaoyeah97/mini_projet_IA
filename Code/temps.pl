% gvim:  fileencoding=utf8

/** <module> Quelques éléments temporels
 *
 * Ce module fournit quelques prédicats simple autour de la gestion d'un horaire constitué d'une heure et de minutes.
 * Pour l'application de co-bicyclage, on pourrait se contenter d'entiers représentant des minutes.
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
 * - load this module:  "consult('NAME_THE_MODULE.pl').";
 * - start (a new tab on) on your Web browser (with javascript enabled):  "doc_browser.".
 * - "Et voilà!"
 *
 */

:- module(temps, [ duree/1
                 , random_duree_between/3
                 , horaire/1
                 , random_horaire/1
                 , horaire_inferieur/2
                 , horaire_inferieur_egal/2
                 , horaire_between/3
                 , random_horaire_between/3
                 , horaire_minute/2
                 , difference_horaire/3
                 ]).

%! duree(?D:  integer) is det.
%! duree(-D:  integer) is nondet.
% 
% Contrôle de la validité d'une durée ou génération de telles durées.
%
% @arg D  Une durée positive, exprimée en minutes entières.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, la durée est exprimée en minutes entières, est strictement positive et même inférieure ou égale à 120.
%
% @bug  Imposer une durée maximale est lié à l'application (et à la génération) et on devrait se contenter de la positivité (et de l'entier maximal représentable) si ce prédicat était placé dans une bibliothèque plus générale.
%
duree(D) :-
   integer(D),         % pour que le prédicat fonctionne aussi bien en contrôle...
   between(0, 200, D),
   !.                  % (pour éviter un comportement semidet)
duree(D) :-
   var(D),             % ... qu'en génération
   between(0, 200, D).

:- begin_tests(duree_ad_hoc).

test(un_cas_vrai) :-
   assertion( duree(0) ).

test(un_autre_cas_vrai) :-
   assertion( duree(8) ).

test(un_cas_faux) :-
   assertion( \+ duree(-4) ).

test(un_autre_cas_faux) :-
   assertion( \+ duree(3.14159) ).

test(un_dernier_cas_faux) :-
   assertion( \+ duree("coucou") ).

:- end_tests(duree_ad_hoc).

%! random_duree_between(+T_min:  duree,  +T_max:  duree,  ?T:  duree) is det.
%
% Génération, ou contrôle, d'une durée.
%
% @arg T_min   Une durée.
% @arg T_max   Une autre durée.
% @arg T       Une urée entière aléatoire comprise dans l'intervalle défini par les deux bornes, s'il n'est pas vide.
%
% @throws Postcondition.   La durée (fixée ou aléatoire) est comprise entre les deux bornes, si l'intervalle n'est pas vide.
%
random_duree_between(T_min, T_max, T) :-
   integer(T),
   !,
   between(T_min, T_max, T).
random_duree_between(T_min, T_max, T) :-
   var(T),
   random_between(T_min, T_max, T).

:- begin_tests(random_duree_between_ad_hoc).

test(generation_aleatoire) :-
   assertion( ( random_duree_between(0, 200, T)
              , between(0, 200, T)
              ) ).

test(non_generation_vrai) :-
   assertion( random_duree_between(0, 200, 0) ).

test(autre_non_generation_vrai) :-
   assertion( random_duree_between(0, 200, 200) ).

test(non_generation_faux) :-
   assertion( \+ random_duree_between(0, 2000, 2001) ).

test(non_generation_faux) :-
   assertion( \+ random_duree_between(0, 200, "coucou") ).

:- end_tests(random_duree_between_ad_hoc).

%! horaire(+H:  heure(+H, +M)) is det.
%! horaire(-H:  heure(?H, ?M)) is nondet.
% 
% Contrôle de la validité d'un horaire journalier ou génération de tels horaires.
%
% @arg H  Un horaire exprimé en heures et minutes entières, limité à une journée.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, l'heure est un entier compris entre 0 et 23 et la minute, un entier entre 0 et 59.
%
horaire(heure(H, M)) :-
   (integer(H), ! ; var(H)),
   (integer(M), ! ; var(M)),
   between(0, 23, H),
   between(0, 59, M).

:- begin_tests(horaire_ad_hoc).

test(un_cas_vrai) :-
   assertion( horaire(heure(12, 38)) ).

test(un_cas_faux) :-
   assertion( \+ horaire(0) ).

test(un_second_cas_faux) :-
   assertion( \+ horaire(heure(25, 00)) ).

test(un_autre_cas_faux) :-
   assertion( \+ horaire(heure(00, -01)) ).

test(un_dernier_cas_faux) :-
   assertion( \+ horaire("coucou") ).

test(un_tout_dernier_cas_faux) :-
   assertion( \+ horaire(heure(3.14159, 2.71828)) ).

:- end_tests(horaire_ad_hoc).

%! random_horaire(+H:  horaire) is det.
%! random_horaire(-H:  horaire) is nondet.
%
% Génération (éventuellement simple vérification) d'un horaire aléatoire journalier.
%
% @arg H   Horaire constitué d'une heure et de minutes.
%
% @throws Postcondition.   L'horaire est journalier, c'est-à-dire l'heure comprise entre 0 et 23 et les minutes entre 0 et 59.
%
random_horaire(heure(H, M)) :-
   integer(H),
   integer(M),
   !,
   between(0, 23, H),
   between(0, 59, M).
random_horaire(heure(H, M)) :-
   var(H),
   integer(M),
   !,
   random_between(0, 23, H),
   between(0, 59, M).
random_horaire(heure(H, M)) :-
   integer(H),
   var(M),
   !,
   between(0, 23, H),
   random_between(0, 59, M).
random_horaire(heure(H, M)) :-
   var(H),
   var(M),
   !,
   random_between(0, 23, H),
   random_between(0, 59, M).

:- begin_tests(random_horaire_ad_hoc).

test(generation_aleatoire_complete) :-
   assertion( forall( between(1, 100, _)
                    , ( random_horaire(H)
                      , horaire(H)
                      )
                    ) ).

test(generation_aleatoire_heure) :-
   assertion( forall( between(1, 100, _)
                    , ( random_horaire(heure(H, 00))
                      , horaire(heure(H, 00))
                      )
                    ) ).

test(generation_aleatoire_minutes) :-
   assertion( forall( between(1, 100, _)
                    , ( random_horaire(heure(09, M))
                      , horaire(heure(09, M))
                      )
                    ) ).

test(non_generation_vrai) :-
   assertion( random_horaire(heure(9, 13)) ).

test(non_generation_faux) :-
   assertion( \+ random_horaire(heure("coucou", _)) ).

:- end_tests(random_horaire_ad_hoc).

%! horaire_inferieur(?H_1:  horaire,  ?H_2:  horaire) is nondet.
%
% Comparaison d'ordre, au sens strict, entre deux horaires.
% Il s'agit de l'ordre lexicographique, en primaire sur l'heure, en secondaire sur les minutes.
%
% @arg H_1   Un horaire journalier constitué d'une heure et de minutes.
% @arg H_1   Un second horaire à lui comparer.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, H_1 est bien _strictement_ inférieur à H_1.
%
horaire_inferieur(heure(H_min, M_min), heure(H_max, M_max)) :-
   horaire(heure(H_min, M_min)),
   horaire(heure(H_max, M_max)),
   ( H_min < H_max, !
   ; H_min = H_max, M_min < M_max
   ).

:- begin_tests(horaires_inferieurs_ad_hoc).

test(un_cas_vrai) :-
   assertion( horaire_inferieur(heure(9, 10), heure(10, 30)) ).

test(un_autre_cas_vrai) :-
   assertion( horaire_inferieur(heure(9, 10), heure(9, 30)) ).

test(un_cas_faux) :-
   assertion( \+ horaire_inferieur(heure(23, 59), heure(0, 0)) ).

:- end_tests(horaires_inferieurs_ad_hoc).

%! horaire_inferieur_egal(?H_1:  horaire,  ?H_2:  horaire) is nondet.
%
% Comparaison d'ordre, au sens large, entre deux horaires.
% Il s'agit de l'ordre lexicographique, en primaire sur l'heure, en secondaire sur les minutes.
%
% @arg H_1   Un horaire journalier constitué d'une heure et de minutes.
% @arg H_1   Un second horaire à lui comparer.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, H_1 est bien inférieur ou égal à H_2 depuis minuit jusqu'à minuit moins une.
%
horaire_inferieur_egal(H_1, H_2) :-
   H_1 = H_2,
   !.
horaire_inferieur_egal(H_1, H_2) :-
   horaire_inferieur(H_1, H_2).

%! horaire_between(?H_min:  horaire,  ?H_max:  horaire,  ?H:  horaire) is nondet.
%
% Comparaison, ou génération, d'horaire dans un intervalle horaire, fermé aux deux extrémités.
%
% @arg H_min   Horaire journalier constitué d'une heure et de minutes.
% @arg H_max   Horaire supérieur ou égal à l'horaire minimal.
% @arg H       Un horaire situé entre les horaires extrêmes, en considérant minuit situé au début et minuit moins une à la fin.
%
% @throws Postcondition.   L'horaire est un horaire d'embauche maximal du matin, soit au plus tard à 09:00.
%
horaire_between(H_min, H_max, H) :-
   horaire_inferieur_egal(H_min, H),
   horaire_inferieur_egal(H, H_max).

%! random_horaire_between(+H_min:  horaire,  +H_max:  horaire,  ?H:  horaire) is det.
%
% Génération d'un horaire aléatoire dans un intervalle horaire, fermé aux deux extrémités.
%
% @arg H   Horaire constitué d'une heure et de minutes.
%
% @throws Postcondition.   L'horaire tiré au hasard est bien dans l'intervalle dès lors que l'intervalle n'est pas vide.
%
% @bug   L'implémentation répète des tirages aléatoires jusqu'à ce que la contrainte soit satisfaite.
%        La convergence du prédicat devient probabiliste.
%        En cas d'intervalles restreints, le temps peut considérablement augmenter.
%        Dans le cas extrême, pour un intervalle réduit à une minute particulière, parmi les 24 x 60 possibilité, les chances de tirer cette minute-là au hasard sont de 0,07 %...
%
random_horaire_between(H_min, H_max, H) :-
   horaire_inferieur_egal(H_min, H_max),
   repeat,
      random_horaire(H),                % on choisit aléatoirement un horaire
      horaire_between(H_min, H_max, H), % jusqu'à ce que la contrainte d'appartenance à l'intervalle soit satisfaite.
   !.

:- begin_tests(random_horaire_between).

test(dans_intervalle_horaire) :-
   H_min = heure(7, 20),
   H_max = heure(14, 07),
   assertion( forall( between(1, 100, _)
                    , ( random_horaire_between(H_min, H_max, H)
                      , horaire_between(H_min, H_max, H)
                      )
                    ) ).

:- end_tests(random_horaire_between).

%! horaire_minute(+H:  heure,  -T:  natural) is det.
%! horaire_minute(-H:  heure,  +T:  natural) is det.
% 
% Équivalence entre un horaire quotidien et les minutes correspondantes.
%
% @arg H  Un horaire exprimé en heures et minutes entières, limité à une journée.
% @arg T  La minute temporelle correspond à l'horaire.
%
% @throws Precondition.  L'horaire est un horaire quotidien.
% @throws Precondition.  La minute temporelle est comprise entre 0 et 1439 (c'est-à-dire 23 x 60 + 59).
%
% @throws Postcondition.  Les deux informations sont bien équivalentes, on peut passer de l'une à l'autre.
%
horaire_minute(heure(H, M), T) :-
   integer(H),
   integer(M),
   var(T),
   !,
   horaire(heure(H, M)),
   T is H * 60 + M.
horaire_minute(heure(H, M), T) :-
   integer(T),
   var(H),
   var(M),
   !,
   between(0, 1439, T),
   H is div(T, 60),
   M is mod(T, 60).

:- begin_tests(horaires_minutes).

test(equivalence_temps) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, 1439, T)
                      , horaire_minute(H, T)
                      , horaire_minute(H, T_prime)
                      , T = T_prime
                      )
                    ) ).

test(equivalence_horaire) :-
   assertion( forall( between(1, 100, _)
                    , ( random_horaire(H)
                      , horaire_minute(H, T)
                      , horaire_minute(H_prime, T)
                      , H = H_prime
                      )
                    ) ).

:- end_tests(horaires_minutes).

%! difference_horaire(+H_min:  heure,  +H_max:  heure,  -T:  duree) is det.
%! difference_horaire(+H_min:  heure,  -H_max:  heure,  +T:  duree) is det.
%! difference_horaire(-H_min:  heure,  +H_max:  heure,  +T:  duree) is det.
% 
% Détermination de la durée écoulé entre une heure de départ et une heure d'arrivée, qui doit lui être supérieure.
%
% @arg H_min   Horaire journalier constitué d'une heure et de minutes.
% @arg H_max   Horaire supérieur ou égal à l'horaire minimal.
% @arg T       La durée correspondant à l'intervalle horaire entre H_min et H_max.
%
% @throws Precondition.   Le prédicat est satisfiable dès lors qu'au moins un des deux paramètres est instancié.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, l'heure est un entier compris entre 0 et 23 et la minute, un entier entre 0 et 59.
%
difference_horaire(H_min, H_max, T) :-
   horaire_minute(H_min, T_min),
   horaire_minute(H_max, T_max),
   plus(T_min, T, T_max).
difference_horaire(H_min, H_max, T) :-
   horaire_minute(H_min, T_min),
   plus(T_min, T, T_max),
   horaire_minute(H_max, T_max).
difference_horaire(H_min, H_max, T) :-
   horaire_minute(H_max, T_max),
   plus(T_min, T, T_max),
   horaire_minute(H_min, T_min).

:- begin_tests(differences_horaires).

:- end_tests(differences_horaires).

:- run_tests([ duree_ad_hoc
             , random_duree_between_ad_hoc
             , horaire_ad_hoc
             , random_horaire_ad_hoc
             , horaires_inferieurs_ad_hoc
             , random_horaire_between
             , horaires_minutes
             , differences_horaires
             ]).

