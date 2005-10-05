#ifndef _REFLECTION_WRAP_H_
#define _REFLECTION_WRAP_H_

#include <boost/python.hpp>
#include "reflection.h"

using namespace boost::python;

class Reflection_wrap : public Reflection
{
  public:
    Reflection_wrap();  
    Reflection_wrap(Reflection const& r);
  
    list getAngles();
};

#endif
