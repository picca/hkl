#ifndef _CONFIG_H_
#define _CONFIG_H_

#if ( _MSC_VER && _MSC_VER <= 1200 )
  #define VCPP6
  // Ce pragma ne devrai pas être dans ce fichier car le warning ne s'affiche
  // que si l'on compile une version DEBUG de la librairie.
  // Il faut utiliser l'option de compilation \wd4786 mais elle n'est présente
  // qu'à partir de VC++7 
  #pragma warning (disable : 4786)
#endif

#endif //_CONFIG_H_
