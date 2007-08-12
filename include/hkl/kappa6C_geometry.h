#ifndef _KAPPA6C_GEOMETRY_H
#define _KAPPA6C_GEOMETRY_H


#include "geometry_kappa.h"
#include "axe_rotation.h"
#include "HKLException.h"

namespace hkl { namespace twoC { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace eulerian4C { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace kappa4C { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace eulerian6C { class Geometry; }  } 

namespace hkl {

namespace kappa6C {

class Geometry : public hkl::geometry::Kappa {
  protected:
    hkl::axe::Rotation * _mu;

    hkl::axe::Rotation * _komega;

    hkl::axe::Rotation * _kappa;

    hkl::axe::Rotation * _kphi;

    hkl::axe::Rotation * _gamma;

    hkl::axe::Rotation * _delta;


  public:
    /**
     *  @brief Default constructor
     */
    Geometry();

    /**
     *  @brief Another constructor.
     *  @param mu the first angle value.
     *  @param komega the second angle value.
     *  @param kappa the third angle value.
     *  @param kphi the fourth angle value.
     *  @param gamma the fifth angle value.
     *  @param delta the sixth angle value.
     */
    Geometry(double mu, double komega, double kappa, double kphi, double gamma, double delta);

    virtual ~Geometry();

    /**
     * @brief Copy Constructor.
     */
    Geometry(const Geometry & geometry);

    /**
     * @brief Get the _mu Axe.
     * @return A pointer on the _mu Axe.
     */
    hkl::axe::Rotation * mu();

    /**
     * @brief Get the _komega Axe.
     * @return A pointer on the _komega Axe.
     */
    hkl::axe::Rotation * komega();

    /**
     * @brief Get the _kappa Axe.
     * @return A pointer on the _kappa Axe.
     */
    hkl::axe::Rotation * kappa();

    /**
     * @brief Get the _kphi Axe.
     * @return A pointer on the _kphi Axe.
     */
    hkl::axe::Rotation * kphi();

    /**
     * @brief Get the _gamma Axe.
     * @return A pointer on the _gamma Axe.
     */
    hkl::axe::Rotation * gamma();

    /**
     * @brief Get the _delta Axe.
     * @return A pointer on the _delta Axe.
     */
    hkl::axe::Rotation * delta();

    /**
     * @brief Get the _mu Axe.
     * @return A pointer on the _mu Axe.
     */
    const hkl::axe::Rotation * mu() const;

    /**
     * @brief Get the _komega Axe.
     * @return A pointer on the _komega Axe.
     */
    const hkl::axe::Rotation * komega() const;

    /**
     * @brief Get the _kappa Axe.
     * @return A pointer on the _kappa Axe.
     */
    const hkl::axe::Rotation * kappa() const;

    /**
     * @brief Get the _kphi Axe.
     * @return A pointer on the _kphi Axe.
     */
    const hkl::axe::Rotation * kphi() const;

    /**
     * @brief Get the _gamma Axe.
     * @return A pointer on the _gamma Axe.
     */
    const hkl::axe::Rotation * gamma() const;

    /**
     * @brief Get the _delta Axe.
     * @return A pointer on the _delta Axe.
     */
    const hkl::axe::Rotation * delta() const;

    /**
     * @brief Set the angles of the eulerian4CD::Vertical geometry. 
     * @param mu The value of the "omega" Axe.
     * @param komega The value of the "chi" Axe.
     * @param kappa The value of the "phi" Axe.
     * @param kphi The value of the "2theta" Axe.
     * @param gamma The value of the "gamma" Axe.
     * @param delta The value of the "delta" Axe.
     */
    void setAngles(double mu, double komega, double kappa, double kphi, double gamma, double delta);

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::twoC::vertical::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::eulerian4C::vertical::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::kappa4C::vertical::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::eulerian6C::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

};

} // namespace hkl::kappa6C

} // namespace hkl
#endif
