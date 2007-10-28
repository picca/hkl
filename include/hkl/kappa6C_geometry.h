#ifndef _KAPPA6C_GEOMETRY_H
#define _KAPPA6C_GEOMETRY_H

#include "geometry_kappa.h"
#include "axis.h"
#include "HKLException.h"

namespace hkl { namespace twoC { namespace vertical { class Geometry; } } }
namespace hkl { namespace eulerian4C { namespace vertical { class Geometry; } } }
namespace hkl { namespace kappa4C { namespace vertical { class Geometry; } } }
namespace hkl { namespace eulerian6C { class Geometry; } }

namespace hkl
{

	namespace kappa6C
	{

		class Geometry : public hkl::geometry::Kappa
		{
			protected:
				hkl_axis * _mu;

				hkl_axis * _komega;

				hkl_axis * _kappa;

				hkl_axis * _kphi;

				hkl_axis * _gamma;

				hkl_axis * _delta;


			public:
				/**
				 * @brief Default constructor
				 * @param alpha The alpha angle of the kappa geometry.
				 */
				Geometry(double alpha);

				/**
				 * @brief Another constructor.
				 * @param alpha the alpha parameter of the kappa geometry.
				 * @param mu the first angle value.
				 * @param komega the second angle value.
				 * @param kappa the third angle value.
				 * @param kphi the fourth angle value.
				 * @param gamma the fifth angle value.
				 * @param delta the sixth angle value.
				 */
				Geometry(double alpha, double mu, double komega, double kappa, double kphi, double gamma, double delta);

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
				 * @brief Get the _komega Axe.
				 * @return A pointer on the _komega Axe.
				 */
				hkl_axis * komega();

				/**
				 * @brief Get the _kappa Axe.
				 * @return A pointer on the _kappa Axe.
				 */
				hkl_axis * kappa();

				/**
				 * @brief Get the _kphi Axe.
				 * @return A pointer on the _kphi Axe.
				 */
				hkl_axis * kphi();

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
				 * @brief Get the _komega Axe.
				 * @return A pointer on the _komega Axe.
				 */
				const hkl_axis * komega() const;

				/**
				 * @brief Get the _kappa Axe.
				 * @return A pointer on the _kappa Axe.
				 */
				const hkl_axis * kappa() const;

				/**
				 * @brief Get the _kphi Axe.
				 * @return A pointer on the _kphi Axe.
				 */
				const hkl_axis * kphi() const;

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
				 * @param mu The value of the "mu" Axe.
				 * @param komega The value of the "komega" Axe.
				 * @param kappa The value of the "kappa" Axe.
				 * @param kphi The value of the "kphi" Axe.
				 * @param gamma The value of the "gamma" Axe.
				 * @param delta The value of the "delta" Axe.
				 */
				void set_angles(double mu, double komega, double kappa, double kphi, double gamma, double delta);

				/**
				 * @brief Set the angles of the eulerian4CD::Vertical geometry.
				 * @param mu The value of the "mu" Axe.
				 * @param komega The value of the "komega" Axe.
				 * @param kappa The value of the "kappa" Axe.
				 * @param kphi The value of the "kphi" Axe.
				 * @param gamma The value of the "gamma" Axe.
				 * @param delta The value of the "delta" Axe.
				 */
				void set_angles_consign(double mu, double komega, double kappa, double kphi, double gamma, double delta);

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
