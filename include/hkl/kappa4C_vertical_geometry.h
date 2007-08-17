#ifndef _KAPPA4C_VERTICAL_GEOMETRY_H
#define _KAPPA4C_VERTICAL_GEOMETRY_H


#include "geometry_kappa.h"
#include "HKLException.h"

namespace hkl { namespace axe { class Rotation; }  } 
namespace hkl { namespace twoC { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace eulerian4C { namespace vertical { class Geometry; }  }  } 
namespace hkl { namespace eulerian6C { class Geometry; }  } 
namespace hkl { namespace kappa6C { class Geometry; }  } 

namespace hkl {

namespace kappa4C {

namespace vertical {

class Geometry : public hkl::geometry::Kappa {
  protected:
    hkl::axe::Rotation * _komega;

    hkl::axe::Rotation * _kappa;

    hkl::axe::Rotation * _kphi;

    hkl::axe::Rotation * _tth;


  public:
    /**
     * @brief Default constructor
     * @param alpha the alpha angle of the kappa geometry
     */
    Geometry(double alpha);

    /**
     * @brief Another constructor.
     * @param alpha the alpha angle of the kappa geometry.
     * @param komega the first angle value.
     * @param kappa the second angle value.
     * @param kphi the third angle value.
     * @param tth the fourth angle value.
     */
    Geometry(double alpha, double komega, double kappa, double kphi, double tth);

    virtual ~Geometry();

    /**
     * @brief Copy Constructor.
     */
    Geometry(const Geometry & geometry);

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
     * @brief Get the _tth Axe.
     * @return A pointer on the _tth Axe.
     */
    hkl::axe::Rotation * tth();

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
     * @brief Get the _tth Axe.
     * @return A pointer on the _tth Axe.
     */
    const hkl::axe::Rotation * tth() const;

    /**
     * @brief Set the angles of the eulerian4CD::Vertical geometry. 
     * @param komega The value of the "omega" Axe.
     * @param kappa The value of the "chi" Axe.
     * @param kphi The value of the "phi" Axe.
     * @param tth The value of the "2theta" Axe.
     */
    void setAngles(double komega, double kappa, double kphi, double tth);

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
     * @param geometry The hkl::eulerian6C::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::kappa6C::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

};

} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
#endif
