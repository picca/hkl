#ifndef _CONVENIENCE_H
#define _CONVENIENCE_H

#include <HKLException.h>

namespace hkl
  {
  namespace convenience
    {

    /**
     * @brief return the right angle in between[_pi,pi]
     * @param angle The angle to normalize.
     * @return The angle.
     */
    double normalizeAngle(double angle);

    /*!
     * \brief Compute the atan2 function.
     * \param s The y coordinate of the point P in the xOy plan.
     * \param c The x coordinate of the point P in the xOy plan.
     * \return the angle between Ox and OP.
     */
    double atan2(double s, double c);

    /*!
     * Compute the asin function.
     * \param s The sinus of the angle.
     * \throw HKLException if \f$ /abs(s) > 1\f$.
     * \return The asinus of the angle.
     */
    double asin(double s) throw (HKLException);

  } // namespace convenience
} // namespace hkl

#endif //_CONVENIENCE_H
