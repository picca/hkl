#include "mode.h"

namespace hkl {

  Mode::~Mode(void) {}

  //*************************** PROTECTED FUNCTIONS ****************************************

  Mode::Mode(void) {}

  double
  Mode::_atan2(double s, double c)
  {
    double angle;

    if (fabs(s) < constant::math::epsilon_0) s = 0.;
    if (fabs(c) < constant::math::epsilon_0) c = 0.;
    angle = atan2(s, c);    
    if (fabs(angle) < constant::math::epsilon_0) angle = 0.;
    return angle;
  }

    double
  Mode::_asin(double s) throw (HKLException)
  { 
    double angle;
    if (fabs(s) - 1. > constant::math::epsilon_0)
      throw HKLException("sine bigger than 1.",
                         "",
                         "Mode::_asin");
    else
      angle = asin(s);

    if (fabs(angle) < constant::math::epsilon_0) angle = 0.;

    return angle;
  }

  std::ostream &
  Mode::printToStream(std::ostream& flux) const
  { 
    flux << "Mode: " << get_name() << std::endl;
    return flux;
  }

} // namespace hkl

std::ostream &
operator <<(std::ostream & flux, hkl::Mode const & mode)
{ 
  return mode.printToStream(flux);
}
