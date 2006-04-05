#ifndef _DIFFRACTOMETER_EULERIAN4C_WRAP_H_
#define _DIFFRACTOMETER_EULERIAN4C_WRAP_H_

#include "diffractometer_wrap.h"
#include "diffractometer_eulerian4C.h"

using namespace hkl;
using namespace std;

class Diffractometer_Eulerian4C_wrap : public Diffractometer_wrap
{
  public:
   
    Diffractometer_Eulerian4C_wrap(void);

    virtual ~Diffractometer_Eulerian4C_wrap(void);
};

#endif //_DIFFRACTOMETER_EULERIAN4C_WRAP_H_
