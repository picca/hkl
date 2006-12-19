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

          /**
           * @brief Assign a geometry to another one.
           * @param vertical The geometry to assign.
           * @return the assigned Geometry.
           */
          Vertical & operator=(Vertical const & vertical);

          /**
           * @brief Get the omega axe.
           * @return A pointer on the omega axe.
           */
          Axe * & omega(void)
          {
            return _omega;
          }

          /**
           * @brief Get the chi axe.
           * @return A pointer on the chi axe.
           */
          Axe * & chi(void)
          {
            return _chi;
          }

          /**
           * @brief Get the phi axe.
           * @return A pointer on the phi axe.
           */
          Axe * & phi(void)
          {
            return _phi;
          }

          /**
           * @brief Get the tth axe.
           * @return A pointer on the tth axe.
           */
          Axe * & tth(void)
          {
            return _tth;
          }

          /**
           * @brief Get the omega axe.
           * @return A pointer on the omega axe.
           */
          Axe * const & omega(void) const
            {
              return _omega;
            }

          /**
           * @brief Get the chi axe.
           * @return A pointer on the chi axe.
           */
          Axe * const & chi(void) const
            {
              return _chi;
            }

          /**
           * @brief Get the phi axe.
           * @return A pointer on the phi axe.
           */
          Axe * const & phi(void) const
            {
              return _phi;
            }

          /**
           * @brief Get the tth axe.
           * @return A pointer on the tth axe.
           */
          Axe * const & tth(void) const
            {
              return _tth;
            }

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

          Axe * _omega; //!< the omega Axe.
          Axe * _chi; //!< the chi Axe.
          Axe * _phi; //!< the phi Axe.
          Axe * _tth; //!< the tth Axe.
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

          Axe * _omega; //!< The omega Axe.
          Axe * _chi; //!< The chi Axe.
          Axe * _phi; //!< The phi Axe.
          Axe * _tth; //!< The tth Axe.
        };

    } // namespace eulerian4C
  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
