--------------------------------- Projet HKL -------------------------------------------
-- librairie permettant le calcul des angles moteurs d'un diffractomètre en fonction  --
-- des coordonnées de l'espace réciproque du cristal.                                 --
----------------------------------------------------------------------------------------

- compilé (statique) avec MS VC6 (Makefile.VC, dépend des options de compil tango (cf
fichier tango.opt par exemple)) et gcc 3.2.2 (Makefile).
- diffractomètre 4 cercles : mode bisecteur.
- diffractomètre 6 cercles : modes 4 cercles horizontal et vertical.

- TODO :
pour le 6 cercles, mode 3 cercles bras levant.
pour le 4 cercles, mode omega constant (pb d'architecture : variable omega).

- projets liés :
Tango Device Server "DiffractometerEulerian4Circles".
Système de pilotage d'un diffractomètre 4 cercles au Laboratoire de Physique du Solide.
