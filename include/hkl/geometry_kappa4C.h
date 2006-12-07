#ifndef _GEOMETRY_KAPPA4C_H_
#define _GEOMETRY_KAPPA4C_H_

#include "HKLException.h"
#include "geometry_kappa.h"
#include "geometry_eulerian4C.h"

namespace hkl
  {
  namespace geometry
    {

    class Kappa6C;

    namespace kappa4C
      {

      /**
       * \brief A Geometry for a the Vertical kappa 4 circle soleil generic diffractometer.
       */
      class Vertical : public Kappa
        {
        public:

          /**
           * @brief The default constructor
           *
           * @param alpha The alpha angle of the Kappa geometry.
           */
          Vertical(void);

          /**
           * @brief Constructor initializing the Axes.
           *
           * @param komega The value of the "komega" Axe.
           * @param kappa The value of the "kappa" Axe.
           * @param kphi The value of the "kphi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          Vertical(double komega, double kappa, double kphi, double two_theta);

          /**
           * @brief The copy constructor
           * @param vertical The geometry to copy from.
           */
          Vertical(Vertical const & vertical);

          /**
           * @brief The destructor
           */
          virtual ~Vertical(void);

          Axe * & komega(void)
          {
            return _komega;
          }

          Axe * & kappa(void)
          {
            return _kappa;
          }

          Axe * & kphi(void)
          {
            return _kphi;
          }

          Axe * & tth(void)
          {
            return _tth;
          }

          Axe * const & komega(void) const
            {
              return _komega;
            }

          Axe * const & kappa(void) const
            {
              return _kappa;
            }

          Axe * const & kphi(void) const
            {
              return _kphi;
            }

          Axe * const & tth(void) const
            {
              return _tth;
            }

          /*!
           * \brief Assignation of the Geometry.
           * \param geometry The Geometry to assign.
           */
          Vertical & operator=(Vertical const & geometry);

          /**
           * @brief Set the angles of the kappa4C::Vertical geometry.
           * 
           * @param komega The value of the "komega" Axe.
           * @param kappa The value of the "kappa" Axe.
           * @param kphi The value of the "kphi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          void setAngles(double const & komega, double const & kappa, double const & kphi, double const & two_theta);

          /**
           * @brief Set the kappa4C::Vertical geometry from an other Geometry
           * 
           * @param geometry The Geometry.
           * @param strict
           * @throw HKLException if "chi" > 2 * alpha(kappa). 
           */
          void setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException);

          /**
           * @brief Set the kappa4C::Vertical geometry from an other Geometry
           * 
           * @param geometry The Geometry.
           * @param strict
           * @throw HKLException if "chi" > 2 * alpha(kappa). 
           */
          void setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException);

          /**
           * @brief Set the kappa4C::Vertical geometry from an other Geometry
           * 
           * @param geometry The Geometry.
           * @param strict
           * @throw HKLException if "chi" > 2 * alpha(kappa). 
           */
          void setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException);

          /**
           * @brief Set the kappa4C::Vertical geometry from an other Geometry
           * 
           * @param geometry The Geometry.
           * @param strict
           * @throw HKLException if "chi" > 2 * alpha(kappa). 
           */
          void setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException);

        public:
          Axe * _komega;
          Axe * _kappa;
          Axe * _kphi;
          Axe * _tth;
        };

    } // namespace kappa4C
  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA4C_H_
