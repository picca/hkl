//#include "portability.h"

#include <cmath>
#include "config.h"
#include "convenience.h"

namespace hkl
  {
  namespace convenience
    {

    double
    normalizeAngle(double angle)
    {
      double res = ::atan2(::sin(angle), ::cos(angle));
      if (fabs(res - M_PI) < HKL_EPSILON
          && angle < 0)
        res = -res;

      return res;
    }

    double
    atan2(double s, double c)
    {
      double angle;

      if (fabs(s) < HKL_EPSILON) s = 0.;
      if (fabs(c) < HKL_EPSILON) c = 0.;
      angle = ::atan2(s, c);
      if (fabs(angle) < HKL_EPSILON) angle = 0.;
      return angle;
    }

    double
    asin(double s) throw (HKLException)
    {
      double angle;
      if (fabs(s) - 1. > HKL_EPSILON)
        HKLEXCEPTION("sinus bigger than 1.", "");
      else
        angle = ::asin(s);

      if (fabs(angle) < HKL_EPSILON) angle = 0.;

      return angle;
    }

  } // namespace convenience
} // namespace hkl
