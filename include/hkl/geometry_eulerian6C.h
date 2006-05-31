#ifndef _GEOMETRY_EULERIAN6C_H_
#define _GEOMETRY_EULERIAN6C_H_

#include "geometry.h"

namespace hkl {
  namespace geometry {

    /**
     * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
     */
    class Eulerian6C : public Geometry
    {
      public:

        /**
         * \brief The default constructor
         */
        Eulerian6C(void);

        /** 
         * @brief Constructor specific to the Eulerian 6 circles geometry which set the axes values.
         * 
         * @param mu The "mu" axe value.
         * @param omega  The "omega" axe value.
         * @param chi  The "chi" axe value.
         * @param phi  The "phi" axe value.
         * @param gamma  The "gamma" axe value.
         * @param delta  The "delta" axe value.
         */
        Eulerian6C(double mu, double omega, double chi, double phi, double gamma, double delta);

        /**
         * \brief The destructor
         */
        virtual ~Eulerian6C(void);

        /** 
         * @brief Set the angles of the eulerian6C geometry.
         * 
         * @param mu The value of the "mu" Axe.
         * @param omega The value of the "omega" Axe.
         * @param chi The value of the "chi" Axe.
         * @param phi The value of the "phi" Axe.
         * @param gamma The value of the "gamma" Axe.
         * @param delta The value of the "delta" Axe.
         */
        void setAngles(double const & mu, double const & omega, double const & chi, double const & phi,
                       double const & gamma, double const & delta);

        /** 
         * @brief Set an eulerian6C Geometry from another Geometry.
         * @param geometry The Geometry.
         * @param strict false or true if we must not care of the strictness of the conversion.
         * @throw HKLException depending of the real type of geometry.
         */
        void setFromGeometry(Geometry const & geometry, bool const & strict) throw (HKLException);
    };

  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN6C_H_
