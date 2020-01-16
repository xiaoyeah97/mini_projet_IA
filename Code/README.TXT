---+ Le co-bicyclage : (encore) un problème complexe !

Ce début de programme vise à planifier des déplacements en co-bicyclage, partagés entre un ensemble de propriétaires de cycles prêt à embarquer et débarquer sur des étapes de leur trajet domicile-travail des piétons.

---++ Préalable

Pour mieux visualiser et parcourir la documentation de cette bibliothèque :
   * lancer l'interpréteur Prolog sur la ligne de commande :  "swipl" ;
   * démacer le serveur HTTP de documentation : "doc_server(4000)." ;
   * charger le module qui invoque récursivement tous les autres (les tests unitaires vont être exécutés par la même occasion) :  "consult('cobicyclage_conception.pl')." ;
   * lancer un navigateur (avec JavaScript autorisé de préférence) :  "doc_browser.".
   * Et voilà !

---++ Modules fournis

Une partie du programme est décomposé dans les fichiers interprétables par swi-prolog :
   * utilitaires.pl : des prédicats d'usage général ;
   * temps.pl : une micro-bibliothèque de prédicats associés au temps sous la forme d'horaires journaliers distinguant heure es et minutes ;
   * cobicyclage_analyse.pl : la description formalisée et traduite en Prolog des données du problème ;
   * cobicyclage_instance.pl : une instance simple et _statique_ (c'est-à-dire stockées "en dur") de problème à résoudre basée sur la structure décrite ci-dessus, et pouvant être remplacée par les instances générées ci-dessous ;
   * cobicyclage_random.pl : la génération d'instances de problème aléatoires ;
   * cobicyclage_conception.pl : une solution directe, naïve et... _inutilisable_, pour "résoudre" ce problème complexe.

__Attention.__ Le chargement du module 'cobicyclage_random.pl' lance ses tests unitaires.
Cela consiste en particulier à générer _cent_ instances de problème, chacune décrite par deux fichiers, sa version Prolog, pour remplacer 'cobicyclage_instance.pl' et sa version GraphViz pour la visualiser avec 'xdot'.
Elles viennent "polluer" le répertoire courant !
Heureusement, tous les noms de fichiers débutent par "random_instance", ils peuvent donc être facilement déplacés ou supprimés.

---++ Travail à poursuivre

__Attention.__ Les modules fournis le sont essentiellement à titre illustratif.
Au vu des performances _catastrophiques_ de l'approche décrite dans la conception, il convient de modifier l'approche et notamment de trouver des heuristiques partielles, qui ne garantiront pas de trouver une meilleure solution mais une solution raisonnable dans un laps de temps lui aussi raisonnable.


