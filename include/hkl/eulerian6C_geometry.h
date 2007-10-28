#ifndef _EULERIAN6C_GEOMETRY_H
#define _EULERIAN6C_GEOMETRY_H


#include "geometry.h"
#include "axis.h"
#include "HKLException.h"

namespace hkl { namespace twoC { namespace vertical { class Geometry; } } }
namespace hkl { namespace eulerian4C { namespace vertical { class Geometry; } } }
namespace hkl { namespace kappa4C { namespace vertical { class Geometry; } } }
namespace hkl { namespace kappa6C { class Geometry; } }

namespace hkl
{

	namespace eulerian6C
	{

		class Geometry : public hkl::Geometry
		{
			protected:
				hkl_axis * _mu;

				hkl_axis * _omega;

				hkl_axis * _chi;

				hkl_axis * _phi;

				hkl_axis * _gamma;

				hkl_axis * _delta;


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
				hkl_axis * mu();

				/**
				 * @brief Get the _omega Axe.
				 * @return A pointer on the _omega Axe.
				 */
				hkl_axis * omega();

				/**
				 * @brief Get the _chi Axe.
				 * @return A pointer on the _chi Axe.
				 */
				hkl_axis * chi();

				/**
				 * @brief Get the _phi Axe.
				 * @return A pointer on the _phi Axe.
				 */
				hkl_axis * phi();

				/**
				 * @brief Get the _gamma Axe.
				 * @return A pointer on the _gamma Axe.
				 */
				hkl_axis * gamma();

				/**
				 * @brief Get the _delta Axe.
				 * @return A pointer on the _delta Axe.
				 */
				hkl_axis * delta();

				/**
				 * @brief Get the _mu Axe.
				 * @return A pointer on the _mu Axe.
				 */
				const hkl_axis * mu() const;

				/**
				 * @brief Get the _omega Axe.
				 * @return A pointer on the _omega Axe.
				 */
				const hkl_axis * omega() const;

				/**
				 * @brief Get the _chi Axe.
				 * @return A pointer on the _chi Axe.
				 */
				const hkl_axis * chi() const;

				/**
				 * @brief Get the _phi Axe.
				 * @return A pointer on the _phi Axe.
				 */
				const hkl_axis * phi() const;

				/**
				 * @brief Get the _gamma Axe.
				 * @return A pointer on the _gamma Axe.
				 */
				const hkl_axis * gamma() const;

				/**
				 * @brief Get the _delta Axe.
				 * @return A pointer on the _delta Axe.
				 */
				const hkl_axis * delta() const;

				/**
				 * @brief Set the angles of the eulerian4CD::Vertical geometry.
				 * @param mu The value of the "omega" Axe.
				 * @param omega The value of the "chi" Axe.
				 * @param chi The value of the "phi" Axe.
				 * @param phi The value of the "2theta" Axe.
				 * @param gamma The value of the "gamma" Axe.
				 * @param delta The value of the "delta" Axe.
				 */
				void set_angles(double mu, double omega, double chi, double phi, double gamma, double delta);

				/**
				 * @brief Set the consign angles of the Geometry.
				 * @param mu The value of the "mu" Axe.
				 * @param omega The value of the "omega" Axe.
				 * @param chi The value of the "chi" Axe.
				 * @param phi The value of the "phi" Axe.
				 * @param gamma The value of the "gamma" Axe.
				 * @param delta The value of the "delta" Axe.
				 */
				void set_angles_consign(double mu, double omega, double chi, double phi, double gamma, double delta);

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
