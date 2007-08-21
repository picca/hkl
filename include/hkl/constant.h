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

        static double degToRad;

        static double radToDeg;

    
    };
#define SOLUTION 1
    
    class physic {
      public:
        static double tau;

    };
    
};
inline void eulerian_to_kappa(double const & omega, double const & chi, double const & phi, double const & alpha, double & komega, double & kappa, double & kphi, bool solution = SOLUTION) throw (HKLException)
{
    if (chi < alpha * 2)
      {
        double p = asin(tan(chi/2.)/tan(alpha));

        if (solution)
          {
            komega = omega - p + hkl::constant::math::pi/2.;
            if (komega > hkl::constant::math::pi + hkl::constant::math::epsilon)
              komega -= 2 * hkl::constant::math::pi;
            kappa = 2 * asin(sin(chi/2.)/sin(alpha));
            kphi = phi - p - hkl::constant::math::pi/2.;
            if (kphi <  -hkl::constant::math::pi - hkl::constant::math::epsilon)
              kphi += 2 * hkl::constant::math::pi;
          }
        else
          {
            komega = omega + p - hkl::constant::math::pi/2.;
            if (komega <  -hkl::constant::math::pi - hkl::constant::math::epsilon)
              komega += 2 * hkl::constant::math::pi;
            kappa = -2 * asin(sin(chi/2.)/sin(alpha));
            kphi = phi + p + hkl::constant::math::pi/2.;
            if (kphi > hkl::constant::math::pi + hkl::constant::math::epsilon)
              kphi -= 2 * hkl::constant::math::pi;
          }
#ifdef DEBUG
          std::cout << "omega, chi, phi -> komega, kappa, kphi : (" 
                    << omega * constant::math::radToDeg << ", "
                    << chi * hkl::constant::math::radToDeg << ", "
                    << phi * hkl::constant::math::radToDeg << "), ("
                    << komega * constant::math::radToDeg << ", "
                    << kappa * hkl::constant::math::radToDeg << ", "
                    << kphi * hkl::constant::math::radToDeg << ")" << std::endl;
#endif
      }
    else
        HKLEXCEPTION("Can not set such a Chi value", "must be < 2 * alpha.");
}
inline void kappa_to_eulerian(double const & komega, double const & kappa, double const & kphi, double const & alpha, double & omega, double & chi, double & phi, bool solution = SOLUTION)
{
    double p = atan(tan(kappa/2.) * cos(alpha));

    if (solution)
      {
        omega = komega + p - constant::math::pi/2.;
        if (omega < -hkl::constant::math::pi - hkl::constant::math::epsilon)
          omega += 2 * hkl::constant::math::pi;

        chi = 2 * asin(sin(kappa/2.) * sin(alpha));
        
        phi = kphi + p + constant::math::pi/2.;
        if (phi > hkl::constant::math::pi + hkl::constant::math::epsilon)
          phi -= 2 * hkl::constant::math::pi;
      }
    else
      {
        omega = komega + p + constant::math::pi/2.;
        if (omega > hkl::constant::math::pi + hkl::constant::math::epsilon)
          omega -= 2 * hkl::constant::math::pi;

        chi = -2 * asin(sin(kappa/2.) * sin(alpha));
        phi = kphi + p - constant::math::pi/2.;
        if (phi < -hkl::constant::math::pi - hkl::constant::math::epsilon)
          phi += 2 * hkl::constant::math::pi;
      }
#ifdef DEBUG
    std::cout << "komega, kappa, kphi -> omega, chi, phi : (" 
              << komega * constant::math::radToDeg << ", "
              << kappa * constant::math::radToDeg << ", "
              << kphi * constant::math::radToDeg << "), ("
              << omega * constant::math::radToDeg << ", "
              << chi * hkl::constant::math::radToDeg << ", "
              << phi * hkl::constant::math::radToDeg << ")" << std::endl;
#endif
}

} // namespace hkl
#endif
