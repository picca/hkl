--------------------------------- Projet HKL -------------------------------------------
-- librairie permettant le calcul des angles moteurs d'un diffractomètre en fonction  --
-- des coordonnées de l'espace réciproque du cristal.                                 --
----------------------------------------------------------------------------------------

-COMPILATION:

Prérequis:
  libcppunit version >= 1.10.2  https://sourceforge.net/projects/cppunit/
  scons version >= 0.96.1       http://www.scons.org/

Sous MS VC6
  modifier les PATH pour que windows trouve le programme scons
  modifier le fichier test/SConscript pour que le compilateur trouve les entêtes et la librairie cppunit
  compiler en lançant scons dans le répertoire qui contient SConstruct

Sous linux
  juste tapper scons dans le repertoire contenant le fichier SConstruct.

-TEST:

  Après compilation aller dans le repertoire test et lancer le programme
    windows: libhkl-test.exe
    linux: ./libhkl-test

  Si tout se passe bien vous devriez obtenir un message comme celui-ci:
  
  picca@grisette:~/Projets/hkl/test$ ./libhkl-test
  ..................................................................................

  OK (82)

  Sinon rapporter les problèmes à picca@synchrotron-soleil.fr
  
-TODO :

pour le 6 cercles, Implémenter les modes.
pour le 4 cercles, Implémenter le mode oméga constant, le psy scan.

- projets liés :
Tango Device Server "DiffractometerEulerian4Circles".
Système de pilotage d'un diffractomètre 4 cercles au Laboratoire de Physique du Solide.
