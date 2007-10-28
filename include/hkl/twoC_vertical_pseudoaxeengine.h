#ifndef _TWOC_VERTICAL_PSEUDOAXEENGINE_H
#define _TWOC_VERTICAL_PSEUDOAXEENGINE_H

#include <ostream>

#include "pseudoaxeengine.h"
#include "twoC_vertical_geometry.h"
#include "HKLException.h"
#include "interval.h"

namespace hkl { class PseudoAxe; }

namespace hkl {
	namespace twoC {
		namespace vertical {
			namespace pseudoAxeEngine {

				class Th2th : public hkl::PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>
				{
					protected:
						hkl_axis * _omega;

						double _omega0;

						hkl_axis * _tth;

						double _tth0;

						hkl::PseudoAxe * _th2th;


					public:
						Th2th(hkl::twoC::vertical::Geometry & geometry);

						~Th2th();

						/**
						 * @brief Initialize the pseudoAxe.
						 *
						 * This method must be call before using a pseudoAxe.
						 */
						virtual void initialize() throw(hkl::HKLException);

						virtual void update();

						/**
						 * @brief set the current value of the PseudoAxe.
						 * @throw HKLException if the pseudoAxe is not ready to be set.
						 */
						virtual void set() throw(hkl::HKLException);

				};
				class Q2th : public hkl::PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>
				{
					protected:
						hkl_axis * _omega;

						double _omega0;

						hkl_axis * _tth;

						double _tth0;

						hkl::PseudoAxe * _q2th;


					public:
						Q2th(hkl::twoC::vertical::Geometry & geometry);

						~Q2th();

						/**
						 * @brief Initialize the pseudoAxe.
						 *
						 * This method must be call before using a pseudoAxe.
						 */
						virtual void initialize() throw(hkl::HKLException);

						virtual void update();

						/**
						 * @brief set the current value of the PseudoAxe.
						 * @throw HKLException if the pseudoAxe is not ready to be set.
						 */
						virtual void set() throw(hkl::HKLException);

				};
				class Q : public hkl::PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>
				{
					protected:
						hkl_axis * _tth;

						hkl::PseudoAxe * _q;


					public:
						Q(hkl::twoC::vertical::Geometry & geometry);

						~Q();

						/**
						 * @brief Initialize the pseudoAxe.
						 *
						 * This method must be call before using a pseudoAxe.
						 */
						virtual void initialize() throw(hkl::HKLException);

						virtual void update();

						/**
						 * @brief set the current value of the PseudoAxe.
						 * @throw HKLException if the pseudoAxe is not ready to be set.
						 */
						virtual void set() throw(hkl::HKLException);

						/**
						 * @brief Un-Initialize the pseudoAxe.
						 * This method must be call to un-initialize a pseudoAxe.
						 */
						virtual void uninitialize();

				};

			} // namespace hkl::twoC::vertical::pseudoAxeEngine
		} // namespace hkl::twoC::vertical
	} // namespace hkl::twoC
} // namespace hkl
#endif
