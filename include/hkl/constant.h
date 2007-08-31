#ifndef _CONSTANT_H
#define _CONSTANT_H

#include <cmath>
#include <iostream>

#include "HKLException.h"

namespace hkl {

class constant {
  public:
    class math {
      public:
        static double epsilon;

        static double tiny;

        static int precision;

        static double pi;

        static double infinity;

        static double degToRad;

        static double radToDeg;

    };
#define SOLUTION 1
    
    class physic {
      public:
        static double tau;

    };
    
};

} // namespace hkl
#endif
