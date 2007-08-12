#ifndef _EULERIAN6C_GEOMETRY_H
#define _EULERIAN6C_GEOMETRY_H


#include "geometry.h"
#include "axe_rotation.h"
#include "HKLException.h"

namespace hkl { namespace twoC { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace eulerian4C { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace kappa4C { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace kappa6C { class Geometry; }  } 

namespace hkl {

namespace eulerian6C {

class Geometry : public hkl::Geometry {
  protected:
    hkl::axe::Rotation * _mu;

    hkl::axe::Rotation * _omega;

    hkl::axe::Rotation * _chi;

    hkl::axe::Rotation * _phi;

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
     *  @param omega the second angle value.
     *  @param chi the third angle value.
     *  @param phi the fourth angle value.
     *  @param gamma the fifth angle value.
     *  @param delta the sixth angle value.
     */
    Geometry(double mu, double omega, double chi, double phi, double gamma, double delta);

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
     * @brief Get the _omega Axe.
     * @return A pointer on the _omega Axe.
     */
    hkl::axe::Rotation * omega();

    /**
     * @brief Get the _chi Axe.
     * @return A pointer on the _chi Axe.
     */
    hkl::axe::Rotation * chi();

    /**
     * @brief Get the _phi Axe.
     * @return A pointer on the _phi Axe.
     */
    hkl::axe::Rotation * phi();

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
     * @brief Get the _omega Axe.
     * @return A pointer on the _omega Axe.
     */
    const hkl::axe::Rotation * omega() const;

    /**
     * @brief Get the _chi Axe.
     * @return A pointer on the _chi Axe.
     */
    const hkl::axe::Rotation * chi() const;

    /**
     * @brief Get the _phi Axe.
     * @return A pointer on the _phi Axe.
     */
    const hkl::axe::Rotation * phi() const;

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
     * @param omega The value of the "chi" Axe.
     * @param chi The value of the "phi" Axe.
     * @param phi The value of the "2theta" Axe.
     * @param gamma The value of the "gamma" Axe.
     * @param delta The value of the "delta" Axe.
     */
    void setAngles(double mu, double omega, double chi, double phi, double gamma, double delta);

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
     * @param geometry The hkl::kappa6C::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

};

} // namespace hkl::eulerian6C

} // namespace hkl
#endif
