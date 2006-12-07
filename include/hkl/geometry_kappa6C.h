#ifndef _GEOMETRY_KAPPA6C_H_
#define _GEOMETRY_KAPPA6C_H_

#include "HKLException.h"
#include "geometry_kappa.h"

namespace hkl
  {
  namespace geometry
    {

    // forward declaration
    namespace twoC
      {
      class Vertical;
    }
    namespace eulerian4C
      {
      class Vertical;
    }
    namespace kappa4C
      {
      class Vertical;
    }
    class Eulerian6C;
    // end forward declaration

    /**
     * \brief A Geometry for a the kappa 6 circle soleil generic diffractometer.
     */
    class Kappa6C : public Kappa
      {
        friend class geometry::twoC::Vertical;
        friend class geometry::eulerian4C::Vertical;
        friend class geometry::kappa4C::Vertical;
        friend class geometry::Eulerian6C;

      public:

        /**
         * \brief The default constructor
         *
         * @param alpha The alpha angle of the Kappa geometry.
         */
        Kappa6C(void);

        /**
         * \brief The copy constructor
         */
        Kappa6C(Kappa6C const & kappa6C);

        /**
         * @brief A constructor which set the axes.
         *
         * @param mu The value of the "mu" Axe
         * @param komega The value of the "komega" Axe
         * @param kappa The value of the "kappa" Axe
         * @param kphi The value of the "kphi" Axe
         * @param gamma The value of the "gamma" Axe
         * @param delta The value of the "delta" Axe
         */
        Kappa6C(double mu, double komega, double kappa, double kphi, double gamma, double delta);

        /**
         * \brief The destructor
         */
        virtual ~Kappa6C(void);

        /*!
         * \brief Assignation of the Geometry.
         * \param geometry The Geometry to assign.
         */
        Kappa6C & operator=(Kappa6C const & geometry);

        Axe * & mu(void)
        {
          return _mu;
        }

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

        Axe * & gamma(void)
        {
          return _gamma;
        }

        Axe * & delta(void)
        {
          return _delta;
        }

        Axe * const & mu(void) const
          {
            return _mu;
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

        Axe * const & gamma(void) const
          {
            return _gamma;
          }

        Axe * const & delta(void) const
          {
            return _delta;
          }

        /**
         * @brief Set the angles of the eulerian6C geometry.
         * 
         * @param mu The value of the "mu" Axe.
         * @param komega The value of the "komega" Axe.
         * @param kappa The value of the "kappa" Axe.
         * @param kphi The value of the "kphi" Axe.
         * @param gamma The value of the "gamma" Axe.
         * @param delta The value of the "delta" Axe.
         */
        void setAngles(double const & mu, double const & komega, double const & kappa, double const & kphi,
                       double const & gamma, double const & delta);

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
        void setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException);

      private:

        Axe * _mu;
        Axe * _komega;
        Axe * _kappa;
        Axe * _kphi;
        Axe * _gamma;
        Axe * _delta;
      };

  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA6C_H_
