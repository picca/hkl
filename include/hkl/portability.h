#ifndef _CONFIG_H_
#define _CONFIG_H_

#define HKL_MAJOR 2
#define HKL_MINOR 3
#define HKL_PATCH 0

#define HKL_VERSION (HKL_MAJOR * 10000 + HKL_MINOR * 100 + HKL_PATCH)

#if ( _MSC_VER && _MSC_VER <= 1200 )
#define MSVC6
// Ce pragma ne devrai pas être dans ce fichier car le warning ne s'affiche
// que si l'on compile une version DEBUG de la librairie.
// Il faut utiliser l'option de compilation \wd4786 mais elle n'est présente
// qu'à partir de VC++7
#pragma warning (disable : 4786)
#endif

#endif //_CONFIG_H_
