#include "portability.h"

#include <cmath>

#include "constants.h"
#include "convenience.h"

#ifdef MSVC6
//# define std
#endif

namespace hkl
  {
  namespace convenience
    {

    double
    normalizeAngle(double angle)
    {
      double res = ::atan2(::sin(angle), ::cos(angle));
      if (fabs(res - constant::math::pi) < constant::math::epsilon_0
          && angle < 0)
        res = -res;

      return res;
    }

    double
    atan2(double s, double c)
    {
      double angle;

      if (fabs(s) < constant::math::epsilon_0) s = 0.;
      if (fabs(c) < constant::math::epsilon_0) c = 0.;
      angle = ::atan2(s, c);
      if (fabs(angle) < constant::math::epsilon_0) angle = 0.;
      return angle;
    }

    double
    asin(double s) throw (HKLException)
    {
      double angle;
      if (fabs(s) - 1. > constant::math::epsilon_0)
        HKLEXCEPTION("sinus bigger than 1.", "");
      else
        angle = ::asin(s);

      if (fabs(angle) < constant::math::epsilon_0) angle = 0.;

      return angle;
    }

  } // namespace convenience
} // namespace hkl
