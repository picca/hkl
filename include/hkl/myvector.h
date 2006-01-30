#ifndef _MYVECTOR_H_
#define _MYVECTOR_H_

#include "config.h"

#ifdef VCPP6
  #include "win32/myvector.h"
#else
  #include "linux/myvector.h"
#endif

#endif //_MYVECTOR_H_
