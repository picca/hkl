#ifndef _MODE_EULERIAN6C_H_
#define _MODE_EULERIAN6C_H_

#include "config.h"

#include "mode.h"
#include "mode_eulerian4C.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace mode {
        namespace eulerian6C {
            namespace eulerian4C {

                /**
                 * @brief This class defines the mode for all the 6 circles Eulerian diffractometers.
                 */
                class Vertical
                  {
                  public:
                    virtual ~Vertical(void); //!< Default destructor

                  protected:
                    mutable geometry::eulerian4C::Vertical m_E4CV; //!< The geometry::eulerian4C::Vertical use for the calculation

                    Vertical(void); //!< Default constructor - protected to make sure this class is abstract.

                  };

                namespace vertical {

                    /**
                     * @brief Using an eulerian 6-circle diffractometer as an horizontal 4C eulerian one in bisector mode.
                     *
                     * The eulerian 6-circle diffractometer in horizontal bisector mode as described in <BR>
                     * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
                     * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
                     * In this mode delta = eta = 0, so the scattering vector formula becomes :<BR>
                     * Q = ||Q|| * (0., -sin(theta), cos(theta)) where theta comes from the Bragg relation :<BR>
                     * 2 * tau * sin(theta) = ||Q|| * lambda
                     */
                    class Bissector :
                      public mode::eulerian6C::eulerian4C::Vertical,
#ifdef MSVC6
                      public Mode
#else
                      public mode::eulerian4C::vertical::Bissector
#endif
                    {
                    public:

                      Bissector(void); //!< Default constructor.

                      virtual ~Bissector(void); //!< Default destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Bissector m_mode;
#endif
                    };

                    class Delta_Theta :
                      public mode::eulerian6C::eulerian4C::Vertical,
#ifdef MSVC6
                      public Mode
#else
                      public mode::eulerian4C::vertical::Delta_Theta
#endif
                    {
                    public:

                      Delta_Theta(void); //!< Default constructor.

                      virtual ~Delta_Theta(void); //!< Default destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Delta_Theta m_mode;
#endif
                    };

                    class Constant_Omega :
                      public mode::eulerian6C::eulerian4C::Vertical,
#ifdef MSVC6
                      public Mode
#else
                      public mode::eulerian4C::vertical::Constant_Omega
#endif
                    {
                    public:

                      Constant_Omega(void); //!< Default constructor.

                      virtual ~Constant_Omega(void); //!< Default destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Constant_Omega m_mode;
#endif
                    };

                    class Constant_Chi :
                      public mode::eulerian6C::eulerian4C::Vertical,
#ifdef MSVC6
                      public Mode
#else
                      public mode::eulerian4C::vertical::Constant_Chi
#endif
                    {
                    public:

                      Constant_Chi(void); //!< Default constructor.

                      virtual ~Constant_Chi(void); //!< Default destructor.

                      void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable mode::eulerian4C::vertical::Constant_Chi m_mode;
#endif
                    };

                    class Constant_Phi :
                      public mode::eulerian6C::eulerian4C::Vertical,
#ifdef MSVC6
                      public Mode
#else
                      public mode::eulerian4C::vertical::Constant_Phi
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

            /*!
             * @brief The eulerian 6-circle diffractometer as a 3-circles lifting detector geometry.
             *
             * The eulerian 6-circle diffractometer in 3-circles lifting detector mode. We solve equations described in <BR>
             * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
             * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
             * In this mode eta = chi = phi = 0. <BR>
             * To move the crystal we only use the mu circle, to move the detector we use both delta and nu. <BR>
             * The scattering vector is :<BR>
             * Q = (tau/lambda) * (sin(delta), cos(nu)*cos(delta)-1, sin(nu)*cos(delta))
             */
            class lifting3CDetector : public Mode
            {
            public:

              lifting3CDetector(void); //!< Default constructor.

              virtual ~lifting3CDetector(void); //!< Default destructor.

              /*!
               * @brief The main function to get a sample of angles from (h,k,l).
               * @param h The scaterring vector first element.
               * @param k The scaterring vector second element.
               * @param l The scaterring vector third element.
               * @param UB The product of the orientation matrix U by the crystal matrix B.
               * @param[out] geometry The Geometry to calculate.
               *
               * Solving equation (11) from :
               * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
               * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
               * MU.U.B.(h,k,l) = Q <BR>
               * In this mode :
               * - eta = chi = phi = 0.
               * - delta = arcsin(hphi1 / kk) where kk = tau/lambda
               * - nu = arccos[(1-Q²/kk²)/(2cos(delta))]
               * - sin(mu)*(hphi2²+hphi3²) = -hphi3*kk*(cos(delta)*cos(nu)-1)+hphi2*kk*sin(nu)*cos(delta)
               * - cos(mu)*(hphi2²+hphi3²) =  hphi2*kk*(cos(delta)*cos(nu)-1)+hphi3*kk*sin(nu)*cos(delta)
               * @sa diffractometer_Eulerian4C::test_eulerian4C()
               */
              void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException);
            };

        } // namespace eulerian6C
    } // namespace mode
} // namespace hkl

#endif // _MODE_EULERIAN6C_H_
