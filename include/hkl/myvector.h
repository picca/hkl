#ifndef _MYVECTOR_H_
#define _MYVECTOR_H_

#include "config.h"

#ifdef MSVC6
  #include "win32/myvector.h"
#else
  #include "linux/myvector.h"
#endif

#endif //_MYVECTOR_H_
