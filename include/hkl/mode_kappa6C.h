#ifndef _MODE_KAPPA6C_H_
#define _MODE_KAPPA6C_H_

#include "mode.h"
#include "mode_eulerian4C.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace mode {
        namespace kappa6C {
            namespace eulerian4C {

                /** 
                 * @brief This class defines the mode for all the 6 circles Kappa diffractometers used as a Vertical Eulerian 4 circles.
                 */
                class Vertical : public virtual Mode
                {
                public:

                  virtual ~Vertical(void); //!< Default destructor

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const = 0;

                protected:

                  Vertical(void); //!< Default constructor - protected to make sure this class is abstract.

                  mutable geometry::Kappa6C * m_geometry_K6C; //!< The geometry use to do conversion between E4C and K6C.

                  mutable geometry::eulerian4C::Vertical m_geometry_E4C; //!< The Geometry_Eulerian4C use for the calculation
                };

                namespace vertical {

                    /*!
                     * @brief Using a kappa 6-circle diffractometer as a vertical 4C eulerian one in bisector mode.
                     *
                     * The eulerian 6-circle diffractometer in horizontal bisector mode as described in <BR>
                     * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
                     * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
                     * In this mode delta = eta = 0, so the scattering vector formula becomes :<BR>
                     * Q = ||Q|| * (0., -sin(theta), cos(theta)) where theta comes from the Bragg relation :<BR>
                     * 2tau * sin(theta) = ||Q|| * lambda
                     * 
                     */
                    class Bissector : public mode::eulerian4C::vertical::Bissector, public Vertical
                    {
                    public:

                      Bissector(void); //!< Default constructor.

                      virtual ~Bissector(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
                    };

                    class Delta_Theta : public mode::eulerian4C::vertical::Delta_Theta, public Vertical
                    {
                    public:

                      Delta_Theta(void); //!< Default constructor.

                      virtual ~Delta_Theta(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
                    };

                    class Constant_Omega : public mode::eulerian4C::vertical::Constant_Omega, public Vertical
                    {
                    public:

                      Constant_Omega(void); //!< Default constructor.

                      virtual ~Constant_Omega(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
                    };

                    class Constant_Chi : public mode::eulerian4C::vertical::Constant_Chi, public Vertical
                    {
                    public:

                      Constant_Chi(void); //!< Default constructor.

                      virtual ~Constant_Chi(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
                    };

                    class Constant_Phi : public mode::eulerian4C::vertical::Constant_Phi, public Vertical
                    {
                    public:

                      Constant_Phi(void); //!< Default constructor.

                      virtual ~Constant_Phi(void); //!< Default destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
                    };

                } // namespace vertical
            } // namespace eulerian4C
        } // namespace kappa6C
    } // namespace mode
} // namespace hkl

#endif // _MODE_KAPPA6C_H_
