#ifndef _MYMAP_H_
#define _MYMAP_H_

#include "config.h"

#ifdef MSVC6
  #include "win32/mymap.h"
#else
  #include "linux/mymap.h"
#endif

#endif //_MYMAP_H_
