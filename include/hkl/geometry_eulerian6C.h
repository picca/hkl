#ifndef _GEOMETRY_EULERIAN6C_H_
#define _GEOMETRY_EULERIAN6C_H_

#include "geometry.h"

namespace hkl
{
namespace geometry
{

// begining of forward declaration
namespace twoC
{
class Vertical;
}
namespace eulerian4C
{
class Vertical;
}
namespace kappa4C
{
class Vertical;
}
class Kappa6C;
// end of forward declaration

/**
 * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
 */
class Eulerian6C : public Geometry
{
public:

    /**
     * \brief The default constructor
     */
    Eulerian6C(void);

    /**
     * \brief The copy constructor
     */
    Eulerian6C(Eulerian6C const & geometry);

    /**
     * @brief Constructor specific to the Eulerian 6 circles geometry which set the axes values.
     * 
     * @param mu The "mu" axe value.
     * @param omega  The "omega" axe value.
     * @param chi  The "chi" axe value.
     * @param phi  The "phi" axe value.
     * @param gamma  The "gamma" axe value.
     * @param delta  The "delta" axe value.
     */
    Eulerian6C(double mu, double omega, double chi, double phi, double gamma, double delta);

    /**
     * \brief The destructor
     */
    virtual ~Eulerian6C(void);

    /*!
     * \brief Assignation of the Geometry.
     * \param geometry The Geometry to assign.
     */
    Eulerian6C & operator=(Eulerian6C const & geometry);

    Axe * & mu(void)
    {
        return _mu;
    }

    Axe * & omega(void)
    {
        return _omega;
    }

    Axe * & chi(void)
    {
        return _chi;
    }

    Axe * & phi(void)
    {
        return _phi;
    }

    Axe * & gamma(void)
    {
        return _gamma;
    }

    Axe * & delta(void)
    {
        return _delta;
    }

    Axe * const & mu(void) const
    {
        return _mu;
    }

    Axe * const & omega(void) const
    {
        return _omega;
    }

    Axe * const & chi(void) const
    {
        return _chi;
    }

    Axe * const & phi(void) const
    {
        return _phi;
    }

    Axe * const & gamma(void) const
    {
        return _gamma;
    }

    Axe * const & delta(void) const
    {
        return _delta;
    }

    /**
     * @brief Set the angles of the eulerian6C geometry.
     * 
     * @param mu The value of the "mu" Axe.
     * @param omega The value of the "omega" Axe.
     * @param chi The value of the "chi" Axe.
     * @param phi The value of the "phi" Axe.
     * @param gamma The value of the "gamma" Axe.
     * @param delta The value of the "delta" Axe.
     */
    void setAngles(double const & mu, double const & omega, double const & chi, double const & phi,
                   double const & gamma, double const & delta);

    /**
     * @brief Set an eulerian6C Geometry from another Geometry.
     * @param geometry The Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException depending of the real type of geometry.
     */
    void setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException);

    /**
     * @brief Set an eulerian6C Geometry from another Geometry.
     * @param geometry The Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException depending of the real type of geometry.
     */
    void setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException);

    /**
     * @brief Set an eulerian6C Geometry from another Geometry.
     * @param geometry The Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException depending of the real type of geometry.
     */
    void setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException);

    /**
     * @brief Set an eulerian6C Geometry from another Geometry.
     * @param geometry The Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException depending of the real type of geometry.
     */
    void setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException);

private:
    Axe * _mu;
    Axe * _omega;
    Axe * _chi;
    Axe * _phi;
    Axe * _gamma;
    Axe * _delta;
};

} // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN6C_H_
