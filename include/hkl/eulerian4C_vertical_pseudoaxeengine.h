#ifndef _EULERIAN4C_VERTICAL_PSEUDOAXEENGINE_H
#define _EULERIAN4C_VERTICAL_PSEUDOAXEENGINE_H


#include "pseudoaxeengine.h"
#include "svector.h"
#include "quaternion.h"
#include "range.h"
#include "eulerian4C_vertical_geometry.h"
#include "HKLException.h"
#include <iostream>
using namespace std;
#include "derived_pseudoaxeengine.h"
#include "twoC_vertical_pseudoaxeengine.h"

namespace hkl { namespace axe { class Rotation; }  } 
namespace hkl { class Parameter; } 
namespace hkl { class PseudoAxe; } 
namespace hkl { class SampleList; } 

namespace hkl {

namespace eulerian4C {

namespace vertical {

namespace pseudoAxeEngine {

/**
 * @brief The eulerian 4-circle Vertical diffractometer Psi pseudoAxe.
 *
 * This pseudoAxe represent a rotation around the scattering vector \f$ \vec{Q} \f$.
 *
 * To compute this pseudoAxe we start from the fondamental equation
 * \f[
 *  R = \Omega \chi \phi
 * \f]
 * thus
 * \f[
 *  \left(
 *    \begin{matrix}
 *      \cos\omega \cos\phi - \cos\chi \sin\omega \sin\phi & -\sin\chi \sin\omega & -\cos\omega \sin\phi - \cos\chi \sin\omega \cos\phi \\
 *      -\sin\chi \sin\phi                                 & \cos\chi             & -\sin\chi \cos\phi \\
 *      \sin\omega \cos\phi - \cos\chi \cos\omega \sin\phi & \sin\chi \cos\omega  & -\sin\omega \sin\phi + \cos\chi \cos\omega \cos\phi
 *    \end{matrix}
 *  \right)
 * \f]
 * 
 * Now \f$\psi\f$ is the rotation angle around the scattering vector \f$\vec{Q}\f$.
 * So we can write:
 * \f[
 *  R = \Psi R_0
 * \f]
 * where \f$\Psi\f$ is the rotation matrix around the \f$\vec{Q}\f$ vector
 * and \f$R_0\f$ the \f$R\f$ matrix for \f$\Psi=0\f$
 * 
 * The 1st where \f$\sin\chi > 0\f$ is:
 * \f{eqnarray*}
 *  \omega & = & \arctan(-R_{0,1}, R_{2,1}) \\
 *  \chi & = & \arctan(\sqrt{R_{0,1}^2+R_{2,1}^2}, R_{1,1}) \\
 *  \phi & = & \arctan(-R_{1,0}, -R_{1,2})
 * \f}
 *
 * The 2nd one \f$\sin\chi < 0\f$ is:
 * \f{eqnarray*}
 *  \omega' & = & \arctan(R_{0,1}, -R_{2,1}) \\
 *  \chi' & = & \arctan(-\sqrt{R_{0,1}^2+R_{2,1}^2}, R_{1,1}) = -\chi\\
 *  \phi' & = & \arctan(R_{1,0}, R_{1,2})
 * \f}
 *
 * Thoses two solutions are not valid if \f$\chi=0\f$ or \f$\chi=\pi\f$.
 * In that case the \f$ R\f$ matrix can be simplify:
 * \f[
 *  \left(
 *    \begin{matrix}
 *      \pm\cos(\omega+\phi) & 0 & \mp\sin(\omega+\phi) \\
 *      0                 & \pm1 & 0 \\
 *      \pm\sin(\omega+\phi) & 0 & \pm\cos(\omega+\phi)
 *    \end{matrix}
 *  \right)
 * \f]
 */

class Psi : public hkl::PseudoAxeEngineWithSampleTemp<hkl::eulerian4C::vertical::Geometry> {
  protected:
    hkl::axe::Rotation * _omega;

    hkl::axe::Rotation * _chi;

    hkl::axe::Rotation * _phi;

    hkl::axe::Rotation * _tth;

    hkl::Parameter * _desorientation;

    hkl::svector _Q0;

    hkl::Quaternion _qpsi0;

    hkl::Range _psi_r;

    hkl::Range _psi_w;

    hkl::PseudoAxe * _psi;


  public:
    Psi(hkl::eulerian4C::vertical::Geometry & geometry, hkl::SampleList *& samples);

    ~Psi();

    /**
     * @brief Initialize the pseudoAxe.
     *
     * This method must be call before using a pseudoAxe.
     */
    virtual void initialize() throw(hkl::HKLException);

    /**
     * @brief Un-Initialize the pseudoAxe.
     * This method must be call to un-initialize a pseudoAxe.
     */
    virtual void uninitialize();

    bool isValid() throw(hkl::HKLException);

    virtual void update();

    /**
     * @brief set the current value of the PseudoAxe.
     * @throw HKLException if the pseudoAxe is not ready to be set.
     */
    virtual void set() throw(hkl::HKLException);

    virtual void set_write_from_read();

    /**
     * @brief print on a stream the content of the Psi
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the Psi from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};
typedef hkl::pseudoAxeEngine::Derived<hkl::eulerian4C::vertical::Geometry, hkl::twoC::vertical::pseudoAxeEngine::Th2th> Th2th;
typedef hkl::pseudoAxeEngine::Derived<hkl::eulerian4C::vertical::Geometry, hkl::twoC::vertical::pseudoAxeEngine::Q2th> Q2th;
typedef hkl::pseudoAxeEngine::Derived<hkl::eulerian4C::vertical::Geometry, hkl::twoC::vertical::pseudoAxeEngine::Q> Q;

} // namespace hkl::eulerian4C::vertical::pseudoAxeEngine

} // namespace hkl::eulerian4C::vertical

} // namespace hkl::eulerian4C

} // namespace hkl
#endif
