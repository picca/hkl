#ifndef _DIFFRACTOMETER_KAPPA4C_WRAP_H_
#define _DIFFRACTOMETER_KAPPA4C_WRAP_H_

#include "diffractometer_kappa_wrap.h"

using namespace hkl;
using namespace std;

class Diffractometer_Kappa4C_wrap : public Diffractometer_Kappa_wrap
{
  public:
   
    Diffractometer_Kappa4C_wrap(double alpha);

    virtual ~Diffractometer_Kappa4C_wrap(void);
};

#endif //_DIFFRACTOMETER_KAPPA4C_WRAP_H_
