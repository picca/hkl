#ifndef _GEOMETRY_EULERIAN4C_H_
#define _GEOMETRY_EULERIAN4C_H_

#include "geometry.h"

using namespace std;

namespace hkl
  {
  namespace geometry
    {

    //forward declaration
    namespace twoC
      {
      class Vertical;
    }
    namespace kappa4C
      {
      class Vertical;
      class Horizontal;
    }
    class Eulerian6C;
    class Kappa6C;

    namespace eulerian4C
      {

      /**
       * \brief A Geometry for the Vertical eulerian 4 circle soleil generic diffractometer.
       */
      class Vertical : public Geometry
        {
          friend class hkl::geometry::twoC::Vertical;
          friend class geometry::kappa4C::Vertical;
          friend class geometry::Eulerian6C;
          friend class geometry::Kappa6C;

        public:

          Vertical(void); //!< Default constructor.

          Vertical(Vertical const & geometry); //!< Copy Constructor.

          /**
           * @brief Constructor
           * 
           * @param omega The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          Vertical(double const & omega, double const & chi, double const & phi, double const & two_theta);

          virtual ~Vertical(void); //!< Default destructor.

          Vertical & operator=(Vertical const & vertical);

          /**
           * @brief Set the angles of the eulerian4CD::Vertical geometry.
           * 
           * @param omega The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          void setAngles(double const & omega, double const & chi, double const & phi, double const & two_theta);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException);

        public:

          Axe * _omega;
          Axe * _chi;
          Axe * _phi;
          Axe * _tth;
        };

      /**
       * \brief A Geometry for the Horizontal eulerian 4 circle soleil generic diffractometer.
       */
      class Horizontal : public Geometry
        {
        public:

          Horizontal(void); //!< Default constructor.

          Horizontal(Horizontal const & geometry); //!< Copy Constructor.

          /**
           * @brief Constructor
           * 
           * @param omega  The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          Horizontal(double omega, double chi, double phi, double two_theta);

          virtual ~Horizontal(void); //!< Default destructor.

          /**
           * @brief Set the angles of the eulerian4CD::Horizontal geometry.
           * 
           * @param omega The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          void setAngles(double const & omega, double const & chi, double const & phi, double const & two_theta);

        protected:

          Axe * _omega;
          Axe * _chi;
          Axe * _phi;
          Axe * _tth;
        };

    } // namespace eulerian4C
  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
