#ifndef _GEOMETRY_EULERIAN4C_H_
#define _GEOMETRY_EULERIAN4C_H_

#include "geometry.h"

using namespace std;

namespace hkl {
    namespace geometry {

        //forward declaration
        namespace kappa4C {
            class Vertical;
            class Horizontal;
        }
        class Kappa6C;

        namespace eulerian4C {
            
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
               * @brief Set the angles of the eulerian4CD::Vertical geometry.
               * 
               * @param omega The value of the "omega" Axe.
               * @param chi The value of the "chi" Axe.
               * @param phi The value of the "phi" Axe.
               * @param two_theta The value of the "2theta" Axe.
               */
              void setAngles(double const & omega, double const & chi, double const & phi, double const & two_theta);

              /** 
               * @brief Set an eulerian4C::Vertical Geometry from a kappa4C::Vertical Geometry.
               * @param K4C The kappa4C geometry.
               */
              void setFromGeometry(Geometry const & geometry) throw (HKLException);
            };

            /**
             * \brief A Geometry for the Horizontal eulerian 4 circle soleil generic diffractometer.
             */
            class Horizontal : public Geometry
            { 
            public:

              Horizontal(void); //!< Default constructor.

              Horizontal(Geometry const & geometry); //!< Copy Constructor.

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
            };

        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
