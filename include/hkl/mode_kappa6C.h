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
                class Vertical
                {
                public:

                  virtual ~Vertical(void); //!< Default destructor

                protected:

                  Vertical(void); //!< Default constructor - protected to make sure this class is abstract.

                  mutable geometry::Kappa6C * m_geometry_K6C; //!< The geometry::Kappa6C use for the calculation.
                  
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
#ifdef MSVC6
                    class Bissector : public Mode, public Vertical
#else
                    class Bissector : public mode::eulerian4C::vertical::Bissector, public Vertical
#endif
                    {
                    public:

                      Bissector(void); //!< Default constructor.

                      virtual ~Bissector(void); //!< Default Destructor.

                      virtual void computeAngles(double h, double k, double l,
                                                 smatrix const & UB,
                                                 Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Bissector m_mode;
#endif
                    };

#ifdef MSVC6
                    class Delta_Theta : public Mode, public Vertical
#else            
                    class Delta_Theta : public mode::eulerian4C::vertical::Delta_Theta, public Vertical
#endif                                        
                    {
                    public:

                      Delta_Theta(void); //!< Default constructor.

                      virtual ~Delta_Theta(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Delta_Theta m_mode;
#endif
                    };

#ifdef MSVC6
                    class Constant_Omega : public Mode, public Vertical
#else                                           
                    class Constant_Omega : public mode::eulerian4C::vertical::Constant_Omega, public Vertical
#endif                                           
                    {
                    public:

                      Constant_Omega(void); //!< Default constructor.

                      virtual ~Constant_Omega(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Constant_Omega m_mode;
#endif
                    };

#ifdef MSVC6
                    class Constant_Chi : public Mode, public Vertical
#else                                         
                    class Constant_Chi : public mode::eulerian4C::vertical::Constant_Chi, public Vertical
#endif                                         
                    {
                    public:

                      Constant_Chi(void); //!< Default constructor.

                      virtual ~Constant_Chi(void); //!< Default Destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Constant_Chi m_mode;
#endif
                    };

#ifdef MSVC6
                    class Constant_Phi : public Mode, public Vertical
#else                                         
                    class Constant_Phi : public mode::eulerian4C::vertical::Constant_Phi, public Vertical
#endif                                         
                    {
                    public:

                      Constant_Phi(void); //!< Default constructor.

                      virtual ~Constant_Phi(void); //!< Default destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Constant_Phi m_mode;
#endif
                    };

                } // namespace vertical
            } // namespace eulerian4C
        } // namespace kappa6C
    } // namespace mode
} // namespace hkl

#endif // _MODE_KAPPA6C_H_
