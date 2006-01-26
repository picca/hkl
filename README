--------------------------------- Projet HKL -------------------------------------------
-- librairie permettant le calcul des angles moteurs d'un diffractomètre en fonction  --
-- des coordonnées de l'espace réciproque du cristal.                                 --
----------------------------------------------------------------------------------------

-COMPILATION:

	* Prérequis:
  		libcppunit version >= 1.10.2  https://sourceforge.net/projects/cppunit/
  		scons version >= 0.96.1       http://www.scons.org/

	* Options de compilation avec scons
		debug = 0 ou 1 (scons debug = 1/0)
		profile = 0 ou 1 (scons profile = 0/1)

	* Sous MS VC6
  		modifier les PATH pour que windows trouve le programme scons
  		modifier le fichier test/SConscript pour que le compilateur trouve les entêtes et la librairie cppunit
  		compiler en lançant scons dans le répertoire qui contient SConstruct

	* Sous linux
  		juste tapper scons dans le repertoire contenant le fichier SConstruct.

-TEST:

  Après compilation aller dans le repertoire test et lancer le programme
    windows: libhkl-test.exe
    linux: ./libhkl-test

  Si tout se passe bien vous devriez obtenir un message comme celui-ci:
  
  picca@grisette:~/Projets/hkl/test$ ./libhkl-test
  ..................................................................................

  OK (106)

  Sinon rapporter les problèmes à picca@synchrotron-soleil.fr
