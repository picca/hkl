#ifndef _MODE_EULERIAN6C_H_
#define _MODE_EULERIAN6C_H_

#include "derivedmode.h"
#include "mode_eulerian4C.h"
#include "geometry_eulerian6C.h"

namespace hkl
{
namespace mode
{
namespace eulerian6C
{
namespace eulerian4C
{
namespace vertical
{

typedef DerivedMode<mode::eulerian4C::vertical::Bissector, geometry::Eulerian6C> Bissector;
typedef DerivedMode<mode::eulerian4C::vertical::Delta_Theta, geometry::Eulerian6C> Delta_Theta;
typedef DerivedMode<mode::eulerian4C::vertical::Constant_Omega, geometry::Eulerian6C> Constant_Omega;
typedef DerivedMode<mode::eulerian4C::vertical::Constant_Chi, geometry::Eulerian6C> Constant_Chi;
typedef DerivedMode<mode::eulerian4C::vertical::Constant_Phi, geometry::Eulerian6C> Constant_Phi;

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
class lifting3CDetector : public ModeTemp<geometry::Eulerian6C>
{
public:

    lifting3CDetector(MyString const & name, MyString const & description, geometry::Eulerian6C & geometry); //!< Default constructor.

    virtual ~lifting3CDetector(void); //!< Default destructor.

    /*!
     * @brief The main function to get a sample of angles from (h,k,l).
     * @param h The scaterring vector first element.
     * @param k The scaterring vector second element.
     * @param l The scaterring vector third element.
     * @param UB The product of the orientation matrix U by the crystal matrix B.
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
    void computeAngles(Value const & h, Value const & k, Value const & l,
                       smatrix const & UB) const throw (HKLException);
};

} // namespace eulerian6C
} // namespace mode
} // namespace hkl

#endif // _MODE_EULERIAN6C_H_
