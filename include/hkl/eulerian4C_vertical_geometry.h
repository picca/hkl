#ifndef _EULERIAN4C_VERTICAL_GEOMETRY_H
#define _EULERIAN4C_VERTICAL_GEOMETRY_H

#include "geometry.h"
#include "axis.h"
#include "HKLException.h"

namespace hkl { namespace twoC { namespace vertical { class Geometry; } } }
namespace hkl { namespace kappa4C { namespace vertical { class Geometry; } } }
namespace hkl { namespace eulerian6C { class Geometry; } }
namespace hkl { namespace kappa6C { class Geometry; } }

namespace hkl
{

	namespace eulerian4C
	{

		namespace vertical
		{

			class Geometry : public hkl::Geometry
			{
				protected:
					hkl_axis * _omega;

					hkl_axis * _chi;

					hkl_axis * _phi;

					hkl_axis * _tth;


				public:
					/**
					 *  @brief Default constructor
					 */
					Geometry();

					/**
					 *  @brief Another constructor.
					 *  @param omega the first angle value.
					 *  @param chi the second angle value.
					 *  @param phi the third angle value.
					 *  @param tth the fourth angle value.
					 */
					Geometry(double omega, double chi, double phi, double tth);

					virtual ~Geometry();

					/**
					 * @brief Copy Constructor.
					 */
					Geometry(const Geometry & geometry);

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
					 * @brief Get the _tth Axe.
					 * @return A pointer on the _tth Axe.
					 */
					hkl_axis * tth();

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
					 * @brief Get the _tth Axe.
					 * @return A pointer on the _tth Axe.
					 */
					const hkl_axis * tth() const;

					/**
					 * @brief Set the angles of the eulerian4CD::Vertical geometry.
					 * @param omega The value of the "omega" Axe.
					 * @param chi The value of the "chi" Axe.
					 * @param phi The value of the "phi" Axe.
					 * @param tth The value of the "tth" Axe.
					 */
					void set_angles(double omega, double chi, double phi, double tth);

					/**
					 * @brief Set the angles of the eulerian4CD::Vertical geometry.
					 * @param omega The value of the "omega" Axe.
					 * @param chi The value of the "chi" Axe.
					 * @param phi The value of the "phi" Axe.
					 * @param tth The value of the "tth" Axe.
					 */
					void set_angles_consign(double omega, double chi, double phi, double tth);

					/**
					 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
					 * @param geometry The hkl::twoC::vertical::Geometry.
					 * @param strict false or true if we must not care of the strictness of the conversion.
					 * @throw HKLException
					 */
					void setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

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

					/**
					 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
					 * @param geometry The hkl::kappa6C::Geometry.
					 * @param strict false or true if we must not care of the strictness of the conversion.
					 * @throw HKLException
					 */
					void setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

			};

		} // namespace hkl::eulerian4C::vertical

	} // namespace hkl::eulerian4C

} // namespace hkl
#endif
