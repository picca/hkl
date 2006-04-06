#ifndef _DIFFRACTOMETER_KAPPA_WRAP_H_
#define _DIFFRACTOMETER_KAPPA_WRAP_H_

#include <boost/python.hpp>
#include "diffractometer_wrap.h"

using namespace boost::python;
using namespace hkl;
using namespace std;

class Diffractometer_Kappa_wrap : public Diffractometer_wrap
{
  protected:
    // Constructor
    Diffractometer_Kappa_wrap(double alpha);
};

#endif // _DIFFRACTOMETER_KAPPA_WRAP_H_
