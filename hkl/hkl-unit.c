/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_nan.h>                // for GSL_NAN
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include <stdlib.h>                     // for free, NULL
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-unit-private.h"           // for HklUnit, etc
#include "hkl.h"                        // for FALSE, HKL_DEGTORAD, etc

/**
 * hkl_unit_dup: (skip)
 * @self:
 *
 * copy an #Hklunit
 *
 * Returns: the copied #HklUnit (memory must be release with
 * hkl_unit_free)
 **/
HklUnit* hkl_unit_dup(const HklUnit *self)
{
	if (!self)
		return NULL;

	HklUnit *dup = HKL_MALLOC(HklUnit);
	*dup = *self;

	return dup;
}

/**
 * hkl_unit_free: (skip)
 * @self:
 *
 * release the memory of an #HklUnit
 **/
void hkl_unit_free(HklUnit *self)
{
	if (self)
		free(self);
}

/**
 * hkl_unit_compatible: (skip)
 * @self: the first @HklUnit
 * @unit: the second @HklUnit to check
 *
 * check if two units are compatible.
 *
 * Returns: TRUE or FALSE
 **/
int hkl_unit_compatible(const HklUnit *self, const HklUnit *unit)
{
	int res = TRUE;
	if (self && unit){
		switch(self->type){
		case HKL_UNIT_ANGLE_DEG:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
			case HKL_UNIT_ANGLE_RAD:
			case HKL_UNIT_ANGLE_MRAD:
				break;
			default:
				res = FALSE;
				break;
			}
			break;
		case HKL_UNIT_ANGLE_RAD:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
			case HKL_UNIT_ANGLE_RAD:
			case HKL_UNIT_ANGLE_MRAD:
				break;
			default:
				res = FALSE;
				break;
			}
			break;
		case HKL_UNIT_LENGTH_NM:
			switch(unit->type){
			case HKL_UNIT_LENGTH_NM:
				break;
			default:
				res = FALSE;
				break;
			}
			break;
		case HKL_UNIT_ANGLE_MRAD:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
			case HKL_UNIT_ANGLE_RAD:
			case HKL_UNIT_ANGLE_MRAD:
				break;
			default:
				res = FALSE;
				break;
			}
			break;
		}
	}
	return res;
}

/**
 * hkl_unit_factor:
 * @self:
 * @unit:
 *
 * compute the factor to convert from one @Hklunit to another one.
 * @self * factor =  @unit
 *
 * Returns: the factor of the conversion.
 **/
double hkl_unit_factor(const HklUnit *self, const HklUnit *unit)
{
	double factor = 1.;

	if (self && unit){
		switch(self->type){
		case HKL_UNIT_ANGLE_DEG:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
				break;
			case HKL_UNIT_ANGLE_RAD:
				factor = HKL_DEGTORAD;
				break;
			case HKL_UNIT_ANGLE_MRAD:
				factor = HKL_DEGTORAD * 1e3;
				break;
			default:
				factor = GSL_NAN;
				break;
			}
			break;
		case HKL_UNIT_ANGLE_RAD:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
				factor = HKL_RADTODEG;
				break;
			case HKL_UNIT_ANGLE_RAD:
				break;
			case HKL_UNIT_ANGLE_MRAD:
				factor = 1e3;
				break;
			default:
				factor = GSL_NAN;
				break;
			}
			break;
		case HKL_UNIT_LENGTH_NM:
			switch(unit->type){
			case HKL_UNIT_LENGTH_NM:
				break;
			default:
				factor = GSL_NAN;
				break;
			}
			break;
		case HKL_UNIT_ANGLE_MRAD:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
				factor = 1e-3 * HKL_RADTODEG;
				break;
			case HKL_UNIT_ANGLE_RAD:
				factor = 1e-3;
				break;
			case HKL_UNIT_ANGLE_MRAD:
				break;
			default:
				factor = GSL_NAN;
				break;
			}
			break;
		}
	}
	return factor;
}
