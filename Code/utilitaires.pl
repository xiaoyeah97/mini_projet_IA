% gvim:  fileencoding=utf8

/** <module> Quelques fonctions utilitaires
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
 * - load this module:  "consult('utilitaires.pl').";
 * - start (a new tab on) on your Web browser (with javascript enabled):  "doc_browser.".
 * - "Et voilà!"
 *
 */

:- module(utilitaires, [ all_different/1
                       , all_equal/1
                       , unique/2
                       , random_subset/3
                       , take/3
                       ]).

%! all_different(+XS:  list(expression)) is det.
%
% Vérification de l'absence d'élément dupliqué dans une liste.
%
% @arg XS   Une liste d'éléments comparables par égalité.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, tous les éléments sont distincts les uns des autres.
%
% __Time complexity.__  O(n^2) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
all_different([]).          % dans une liste vide, tous les éléments sont bien différents les uns des autres
all_different([X | XS]) :-  % tandis que dans une liste d'au moins un élément
   \+ member(X, XS),        % il faut que le premier élément de la liste ne se retrouve pas dans la suite de la liste
   all_different(XS).       % et que la suite de la liste ne contienne pas de doublons non plus

:- begin_tests(all_different_ad_hoc).

test(un_cas_vrai) :-
   assertion( all_different([1, 2, 3, a, b, c, "bonjour", "le", "monde"]) ).

test(un_cas_faux) :-
   assertion( \+ all_different([1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]) ).

:- end_tests(all_different_ad_hoc).

%! all_equal(+XS:  list(expression)) is det.
%
% Vérification de la présence d'éléments tous identiques dans une liste.
%
% @arg XS   Une liste d'éléments comparables par égalité.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, tous les éléments sont égaux entre eux.
%
% __Time complexity.__  O(n) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
all_equal([]).             % Dans une liste vide, tous les éléments sont bien égaux les uns des autres.
all_equal([_]).            % C'est également le cas dans une liste singleton.
all_equal([X, X | XS]) :-  % Enfin, c'est le cas si la liste contenant au moins deux éléments commence par un doublet
   all_equal([X | XS]).    % et continue de même.

:- begin_tests(all_equal_ad_hoc).

test(un_cas_faux) :-
   assertion( \+ all_equal([1, 2, 3, a, b, c, "bonjour", "le", "monde"]) ).

test(un_cas_vrai) :-
   assertion( all_equal([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) ).

:- end_tests(all_equal_ad_hoc).

%! unique(+XS:  list(expression), ?YS:  list(expression)) is det.
%
% Suppression des répétitions d'éléments dans une liste.
%
% @arg XS   Une liste d'éléments comparables par égalité.
% @arg YS   La liste des éléments distincts de XS.
%
% @throws Postcondition.   Tous les éléments de YS sont distincts les uns des autres.
% @throws Postcondition.   Tous les éléments de XS sont dans YS et réciproquement.
%
% __Time complexity.__  O(n^2) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
unique([],  []).               % dans une liste vide, il n'y a rien à supprimer
unique([X | XS], YS) :-        % tandis que dans une liste d'au moins un élément
   member(X, XS),              % si le premier élément apparaît aussi plus loin dans la liste
   unique(XS, YS),             % alors cette occurrence n'a pas à apparaître dans le résultat, construit à partir du reste de la liste seulement
   !.                          % (on évite un faux point de retour.)
unique([X | XS], [X | YS]) :-  % sinon, le premier élément est conservé
   unique(XS, YS).             % avec la suite de la liste nettoyée (par hypothèse de récurrence) de ses éventuelles répétitions d'autres éléments.

:- begin_tests(unique).

test(tous_differents) :-
   findall( X
          , ( between(1, 100, _)
            , random_between(1, 20, X)
            )
          , XS), 
   assertion( ( unique(XS, YS)
              , all_different(YS)
              ) ).

test(tous_presents) :-
   findall( X
          , ( between(1, 100, _)
            , random_between(1, 20, X)
            )
          , XS), 
   assertion( ( unique(XS, YS)
              , forall( member(X, XS)
                      , member(X, YS)
                      )
              ) ).

test(tous_presents_reciproque) :-
   findall( X
          , ( between(1, 100, _)
            , random_between(1, 20, X)
            )
          , XS), 
   assertion( ( unique(XS, YS)
              , forall( member(X, YS)
                      , member(X, XS)
                      )
              ) ).

:- end_tests(unique).

%! take(+N:  natural, +XS: list(t), -PS:  list(t)) is semidet.
%
% Retrieves or verifies that PS is the prefix of length N of XS.
% 
% @arg N   Longueur du préfixe.
% @arg XS   Une liste d'éléments.
% @arg PS   Le préfixe de longueur N de XS, s'il existe (en longueur et en valeur)
%
% @bug Recopié depuis la bibliothèque 'xsb_lists' pour éviter d'avoir à la (télé)charger, sans recopier toute la notice!
%
% __Time complexity.__  O(N).
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
take(N, XS, PS) :-      % Un bel exemple de programmation non déterministe !
    length(PS, N),      % PS est un préfixe de taille N
    append(PS, _, XS).  % de XS, s'il apparaît en tête de ce dernier.

%! random_subset(+N:  natural, +XS:  list(expression),  -SS:  list(expression)) is det.
%
% Sélection aléatoire du nombre indiqué d'éléments dans la liste, choisis tous différents les uns des autres.
%
% @arg N    Le nombre d'éléments aléatoires à choisir.
% @arg XS   Une liste d'éléments parmi lesquels choisir des éléments de manière aléatoire.
% @arg SS   La sous-liste d'éléments aléatoires de XS, distincts les uns des autres.
%
% @throws Précondition.   La longueur de la liste d'éléments XS est au moins égale au nombre d'éléments demandés N.
% @throws Précondition.   Tous les éléments de la liste XS sont deux à deux distincts.
%
% @throws Postcondition.   La longueur du résultat SS est égale au nombre d'éléments demandé N.
% @throws Postcondition.   Tous les éléments de SS sont deux à deux distincts.
% @throws Postcondition.   Tous les éléments de SS appartiennent à l'ensemble des éléments fournis XS.
%
% __Time complexity.__  O(n^2) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
random_subset(N, XS, SS) :-  
   assertion( (length(XS, N_XS)
              , N_XS >= N
              ) ),
   assertion( all_different(XS) ),
   random_permutation(XS, PS),  % On permute aléatoirement la liste des éléments fournis,
   take(N, PS, SS).             % et on en récupère les premiers éléments.

:- begin_tests(random_subset).

test(longueur_demandee) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, XS),
   length(XS, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, N_max, N)
                      , random_subset(N, XS, SS)
                      , length(SS, N)
                      )
                    ) ).

test(tous_differents) :-
   atom_chars(abcdefghijklmnopqrstuvwxyz, XS),
   length(XS, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, N_max, N)
                      , random_subset(N, XS, SS)
                      , all_different(SS)
                      )
                    ) ).

test(inclusion) :-
   atom_chars(abcdefghijklmnopqrstuvwxyz, XS),
   length(XS, N_max),
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, N_max, N)
                      , random_subset(N, XS, SS)
                      , subset(SS, XS)
                      )
                    ) ).

:- end_tests(random_subset).

:- run_tests([ all_different_ad_hoc
             , all_equal_ad_hoc
             , unique
             , random_subset
             ]).

