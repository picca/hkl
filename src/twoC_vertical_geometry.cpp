#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl {

	namespace twoC {

		namespace vertical {

			/**
			 *  @brief Default constructor
			 */
			Geometry::Geometry() :
				hkl::Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")
			{
				hkl_holder * holder = NULL;
				static hkl_svector direction = {{1, 0, 0}};
				static hkl_svector axe = {{0., -1., 0.}};

				source.wave_length = HKL_SOURCE_DEFAULT_WAVE_LENGTH;
				source.direction = direction;

				_holders = hkl_holders_new();
				/* sample */
				holder = &_holders->holders[0];
				_omega = hkl_holder_add_rotation_axis(holder, "omega", &axe);

				/* detector */
				holder = &_holders->holders[1];
				_tth = hkl_holder_add_rotation_axis(holder, "tth", &axe);
			}

			/**
			 * @brief Another constructor.
			 * @param omega the first angle value.
			 * @param tth the second angle value.
			 */
			Geometry::Geometry(double omega, double tth) :
				hkl::Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")
			{
				hkl_holder * holder = NULL;
				hkl_svector direction = {{1, 0, 0}};
				hkl_svector axe = {{0., -1., 0.}};

				source.wave_length = HKL_SOURCE_DEFAULT_WAVE_LENGTH;
				source.direction = direction;

				_holders = hkl_holders_new();
				/* sample */
				holder = hkl_holders_add_holder(_holders);
				_omega = hkl_holder_add_rotation_axis(holder, "omega", &axe);

				/* detector */
				holder = hkl_holders_add_holder(_holders);
				_tth = hkl_holder_add_rotation_axis(holder, "tth", &axe);

				this->set_angles(omega, tth);
				this->set_angles_consign(omega, tth);
			}

			Geometry::~Geometry()
			{
			}

			/**
			 * @brief Copy Constructor.
			 */
			Geometry::Geometry(const hkl::twoC::vertical::Geometry & geometry) :
				hkl::Geometry(geometry)
			{
				_omega = &_holders->holders[0].axes->axes[0];
				_tth = &_holders->holders[1].axes->axes[0];
			}

			/**
			 * @brief Get the _omega Axe.
			 * @return A pointer on the _omega Axe.
			 */
			hkl_axis * Geometry::omega()
			{
				return _omega;
			}

			/**
			 * @brief Get the _tth Axe.
			 * @return A pointer on the _tth Axe.
			 */
			hkl_axis * Geometry::tth()
			{
				return _tth;
			}

			/**
			 * @brief Get the _omega Axe.
			 * @return A pointer on the _omega Axe.
			 */
			const hkl_axis * Geometry::omega() const
			{
				return _omega;
			}

			/**
			 * @brief Get the _tth Axe.
			 * @return A pointer on the _tth Axe.
			 */
			const hkl_axis * Geometry::tth() const
			{
				return _tth;
			}

			/**
			 * @brief Set the angles of the eulerian4CD::Vertical geometry.
			 * @param omega The value of the "omega" Axe.
			 * @param tth The value of the "tth" Axe.
			 */
			void Geometry::set_angles(double omega, double tth)
			{
				_omega->config.current = omega;
				_tth->config.current = tth;
			}

			/**
			 * @brief Set the angles of the eulerian4CD::Vertical geometry.
			 * @param omega The value of the "omega" Axe.
			 * @param tth The value of the "tth" Axe.
			 */
			void Geometry::set_angles_consign(double omega, double tth)
			{
				_omega->config.consign = omega;
				_tth->config.consign = tth;
			}

			/**
			 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
			 * @param geometry The hkl::eulerian4C::vertical::Geometry.
			 * @param strict false or true if we must not care of the strictness of the conversion.
			 * @throw HKLException
			 */
			void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
			{
				// check that chi and phi current and consign values are compatible with the convertion in case of a strict conversion
				if (strict) {
					// first the current value
					if (geometry.chi()->config.current != 0 || geometry.phi()->config.current != 0) {
						HKLEXCEPTION("\"chi\" and/or \"phi\" current values are wrong",
								"\"chi\" = \"phi\" current values must be set to zero");
					} else {
						// the the consign values
						if (geometry.chi()->config.consign != 0 || geometry.phi()->config.consign != 0) {
							HKLEXCEPTION("\"chi\" and/or \"phi\" consign values are wrong",
									"\"chi\" = \"phi\" consign values must be set to zero");
						}
					}
				}
				// everything ok so we can set the Geometry.
				source = geometry.source;

				_omega->config.current = geometry.omega()->config.current;
				_tth->config.current = geometry.tth()->config.current;

				_omega->config.consign = geometry.omega()->config.consign;
				_tth->config.consign = geometry.tth()->config.consign;
			}

			/**
			 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
			 * @param geometry The hkl::kappa4C::vertical::Geometry.
			 * @param strict false or true if we must not care of the strictness of the conversion.
			 * @throw HKLException
			 */
			void Geometry::setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
			{
				// check that kappa and kphi current and consign values are compatible with the convertion in case of a strict conversion
				if (strict) {
					// first the current value
					if (geometry.kappa()->config.current != 0 || geometry.kphi()->config.current != 0) {
						HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) current values are wrong",
								"\"kappa\" = \"kphi\" current values must be set to zero");
					} else {
						// the the consign values
						if (geometry.kappa()->config.consign != 0 || geometry.kphi()->config.consign != 0) {
							HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) consign values are wrong",
									"\"kappa\" = \"kphi\" consign values must be set to zero");
						}
					}
				}
				// everything ok so we can set the Geometry.
				source = geometry.source;

				_omega->config.current = geometry.komega()->config.current;
				_tth->config.current = geometry.tth()->config.current;

				_omega->config.consign = geometry.komega()->config.consign;
				_tth->config.consign = geometry.tth()->config.consign;
			}

			/**
			 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
			 * @param geometry The hkl::eulerian6C::Geometry.
			 * @param strict false or true if we must not care of the strictness of the conversion.
			 * @throw HKLException
			 */
			void Geometry::setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException)
			{
				// check that gamma, mu, chi and phi current and consign values are compatible with the convertion
				if (strict) {
					// first the current value
					if (geometry.gamma()->config.current != 0
							|| geometry.mu()->config.current != 0
							|| geometry.chi()->config.current != 0
							|| geometry.phi()->config.current != 0) {
						HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" current values are wrong",
								"\"gamma\" = \"mu\" = \"chi\" = \"phi\" current values must be set to zero");
					} else {
						// the the consign values
						if (geometry.gamma()->config.consign != 0
								|| geometry.mu()->config.consign != 0
								|| geometry.chi()->config.consign != 0
								|| geometry.phi()->config.consign != 0) {
							HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" consign values are wrong",
									"\"gamma\" = \"mu\" = \"chi\" = \"phi\" consign values must be set to zero");
						}
					}
				}
				// ok so set the Geometry
				source = geometry.source;

				_omega->config.current = geometry.omega()->config.current;
				_tth->config.current = geometry.delta()->config.current;

				_omega->config.consign = geometry.omega()->config.consign;
				_tth->config.consign = geometry.delta()->config.consign;
			}

			/**
			 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
			 * @param geometry The hkl::kappa6C::Geometry.
			 * @param strict false or true if we must not care of the strictness of the conversion.
			 * @throw HKLException
			 */
			void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException)
			{
				// check that gamma, mu, kappa and kphi current and consign values are compatible with the convertion
				if (strict) {
					// first the current value
					if (geometry.gamma()->config.current != 0
							|| geometry.mu()->config.current != 0
							|| geometry.kappa()->config.current != 0
							|| geometry.kphi()->config.current != 0) {
						HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" current values are wrong",
								"\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" current values must be set to zero");
					} else {
						// the the consign values
						if (geometry.gamma()->config.consign != 0
								|| geometry.mu()->config.consign != 0
								|| geometry.kappa()->config.consign != 0
								|| geometry.kphi()->config.consign != 0) {
							HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" consign values are wrong",
									"\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" consign values must be set to zero");
						}
					}
				}
				source = geometry.source;

				_omega->config.current = geometry.komega()->config.current;
				_tth->config.current = geometry.delta()->config.current;

				_omega->config.consign = geometry.komega()->config.consign;
				_tth->config.consign = geometry.delta()->config.consign;
			}


		} // namespace hkl::twoC::vertical

	} // namespace hkl::twoC

} // namespace hkl
