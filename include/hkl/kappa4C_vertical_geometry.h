#ifndef _KAPPA4C_VERTICAL_GEOMETRY_H
#define _KAPPA4C_VERTICAL_GEOMETRY_H

#include "config.h"
#include "geometry_kappa.h"
#include "HKLException.h"
#include "portability.h"
#include "interval.h"

/* forwarded declarations */
namespace hkl { namespace axe { class Rotation; } }
namespace hkl { namespace twoC { namespace vertical { class Geometry; } } }
namespace hkl { namespace eulerian4C { namespace vertical { class Geometry; } } }
namespace hkl { namespace eulerian6C { class Geometry; } }
namespace hkl { namespace kappa6C { class Geometry; } }

namespace hkl
  {

  namespace kappa4C
    {

    namespace vertical
      {

      class Geometry : public hkl::geometry::Kappa
        {
        protected:
          hkl::axe::Rotation * _komega;

          hkl::axe::Rotation * _kappa;

          hkl::axe::Rotation * _kphi;

          hkl::axe::Rotation * _tth;


        public:
          /**
           * @brief Default constructor
           * @param alpha the alpha angle of the kappa geometry
           */
          Geometry(double alpha);

          /**
           * @brief Another constructor.
           * @param alpha the alpha angle of the kappa geometry.
           * @param komega the first angle value.
           * @param kappa the second angle value.
           * @param kphi the third angle value.
           * @param tth the fourth angle value.
           */
          Geometry(double alpha, double komega, double kappa, double kphi, double tth);

          virtual ~Geometry();

          /**
           * @brief Copy Constructor.
           */
          Geometry(const Geometry & geometry);

          /**
           * @brief Get the _komega Axe.
           * @return A pointer on the _komega Axe.
           */
          hkl::axe::Rotation * komega();

          /**
           * @brief Get the _kappa Axe.
           * @return A pointer on the _kappa Axe.
           */
          hkl::axe::Rotation * kappa();

          /**
           * @brief Get the _kphi Axe.
           * @return A pointer on the _kphi Axe.
           */
          hkl::axe::Rotation * kphi();

          /**
           * @brief Get the _tth Axe.
           * @return A pointer on the _tth Axe.
           */
          hkl::axe::Rotation * tth();

          /**
           * @brief Get the _komega Axe.
           * @return A pointer on the _komega Axe.
           */
          const hkl::axe::Rotation * komega() const;

          /**
           * @brief Get the _kappa Axe.
           * @return A pointer on the _kappa Axe.
           */
          const hkl::axe::Rotation * kappa() const;

          /**
           * @brief Get the _kphi Axe.
           * @return A pointer on the _kphi Axe.
           */
          const hkl::axe::Rotation * kphi() const;

          /**
           * @brief Get the _tth Axe.
           * @return A pointer on the _tth Axe.
           */
          const hkl::axe::Rotation * tth() const;

          /**
           * @brief Set the angles of the eulerian4CD::Vertical geometry.
           * @param komega The value of the "komega" Axe.
           * @param kappa The value of the "kappa" Axe.
           * @param kphi The value of the "kphi" Axe.
           * @param tth The value of the "tth" Axe.
           */
          void set_angles(double komega, double kappa, double kphi, double tth);

          /**
           * @brief Set the angles of the eulerian4CD::Vertical geometry.
           * @param komega The value of the "komega" Axe.
           * @param kappa The value of the "kappa" Axe.
           * @param kphi The value of the "kphi" Axe.
           * @param tth The value of the "tth" Axe.
           */
          void set_angles_consign(double komega, double kappa, double kphi, double tth);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::twoC::vertical::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::eulerian4C::vertical::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::eulerian6C::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::kappa6C::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException);


        };
      inline void eulerian_to_kappa(double const & omega, double const & chi, double const & phi, double const & alpha, double & komega, double & kappa, double & kphi, bool solution = HKL_EULERIAN_KAPPA_SOLUTION) throw (HKLException)
      {
        if (fabs(chi) <= alpha * 2)
          {
            double p = asin(tan(chi/2.)/tan(alpha));

            if (solution)
              {
                komega = omega - p + M_PI_2;
                //if (komega > hkl::constant::math::pi + hkl::constant::math::epsilon)
                //  komega -= 2 * hkl::constant::math::pi;
                kappa = 2 * asin(sin(chi/2.)/sin(alpha));
                kphi = phi - p - M_PI_2;
                //if (kphi <  -hkl::constant::math::pi - hkl::constant::math::epsilon)
                //  kphi += 2 * hkl::constant::math::pi;
              }
            else
              {
                komega = omega + p - M_PI_2;
                //if (komega <  -hkl::constant::math::pi - hkl::constant::math::epsilon)
                //  komega += 2 * hkl::constant::math::pi;
                kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                kphi = phi + p + M_PI_2;
                //if (kphi > hkl::constant::math::pi + hkl::constant::math::epsilon)
                //  kphi -= 2 * hkl::constant::math::pi;
              }
#ifdef DEBUG
            std::cout << "omega, chi, phi -> komega, kappa, kphi : ("
            << omega * HKL_RADTODEG << ", "
            << chi * hkl::HKL_RADTODEG << ", "
            << phi * hkl::HKL_RADTODEG << "), ("
            << komega * HKL_RADTODEG << ", "
            << kappa * hkl::HKL_RADTODEG << ", "
            << kphi * hkl::HKL_RADTODEG << ")" << std::endl;
#endif
          }
        else
          HKLEXCEPTION("Can not set such a Chi value", "must be < 2 * alpha.");
      }
      inline void kappa_to_eulerian(double const & komega, double const & kappa, double const & kphi, double const & alpha, double & omega, double & chi, double & phi, bool solution = HKL_EULERIAN_KAPPA_SOLUTION)
      {
        double p = atan(tan(kappa/2.) * cos(alpha));

        if (solution)
          {
            omega = komega + p - M_PI_2;
            //if (omega < -hkl::constant::math::pi - hkl::constant::math::epsilon)
            //  omega += 2 * hkl::constant::math::pi;

            chi = 2 * asin(sin(kappa/2.) * sin(alpha));

            phi = kphi + p + M_PI_2;
            //if (phi > hkl::constant::math::pi + hkl::constant::math::epsilon)
            //  phi -= 2 * hkl::constant::math::pi;
          }
        else
          {
            omega = komega + p + M_PI_2;
            //if (omega > hkl::constant::math::pi + hkl::constant::math::epsilon)
            //  omega -= 2 * hkl::constant::math::pi;

            chi = -2 * asin(sin(kappa/2.) * sin(alpha));
            phi = kphi + p - M_PI_2;
            //if (phi < -hkl::constant::math::pi - hkl::constant::math::epsilon)
            //  phi += 2 * hkl::constant::math::pi;
          }
#ifdef DEBUG
        std::cout << "komega, kappa, kphi -> omega, chi, phi : ("
        << komega * HKL_RADTODEG << ", "
        << kappa * HKL_RADTODEG << ", "
        << kphi * HKL_RADTODEG << "), ("
        << omega * HKL_RADTODEG << ", "
        << chi * HKL_RADTODEG << ", "
        << phi * HKL_RADTODEG << ")" << std::endl;
#endif
      }
      inline void kappa_to_eulerian_range(hkl::Range const & komega, hkl::Range const & kappa, hkl::Range const & kphi, double const & alpha, hkl::Interval & omega, hkl::Interval & chi, hkl::Interval & phi, bool solution = HKL_EULERIAN_KAPPA_SOLUTION)
      {
        // compute p = atan(tan(kappa/2) * cos(alpha));
        hkl::Interval p;
        p.set_min(kappa.get_min().get_value());
        p.set_max(kappa.get_max().get_value());
        (((p /= 2).tan()) *= cos(alpha)).atan();

        omega.set_min(komega.get_min().get_value());
        omega.set_max(komega.get_max().get_value());
        chi.set_min(kappa.get_min().get_value());
        chi.set_max(kappa.get_max().get_value());
        phi.set_min(kphi.get_min().get_value());
        phi.set_max(kphi.get_max().get_value());
        if (solution)
          {
            //omega = komega + p - constant::math::pi/2.;
            (omega += p) += -M_PI_2;

            // chi = 2 * asin(sin(kappa/2.) * sin(alpha));
            ((chi /= 2).sin() *= sin(alpha)).asin() *= 2;

            //phi = kphi + p + constant::math::pi/2.;
            (phi += p) += M_PI_2;
          }
        else
          {
            //omega = komega + p + constant::math::pi/2.;
            (omega += p) += M_PI_2;

            //chi = -2 * asin(sin(kappa/2.) * sin(alpha));
            ((chi /= 2).sin() *= sin(alpha)).asin() *= -2;

            //phi = kphi + p - constant::math::pi/2.;
            (phi += p) += -M_PI_2;
          }
#ifdef DEBUG
        std::cout << "komega, kappa, kphi -> omega, chi, phi : ("
        << komega * HKL_RADTODEG << ", "
        << kappa * HKL_RADTODEG << ", "
        << kphi * HKL_RADTODEG << "), ("
        << omega * HKL_RADTODEG << ", "
        << chi * HKL_RADTODEG << ", "
        << phi * HKL_RADTODEG << ")" << std::endl;
#endif
      }

    } // namespace hkl::kappa4C::vertical

  } // namespace hkl::kappa4C

} // namespace hkl
#endif
