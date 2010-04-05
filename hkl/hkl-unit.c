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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <math.h>
#include <gsl/gsl_nan.h>
#include <gsl/gsl_sys.h> /* gsl_nan()! */
#include <hkl/hkl-unit.h>

int hkl_unit_compatible(HklUnit const *self, HklUnit const * unit)
{
	int res = HKL_TRUE;
	if (self && unit){
		switch(self->type){
		case HKL_UNIT_ANGLE_DEG:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
			case HKL_UNIT_ANGLE_RAD:
				break;
			default:
				res = HKL_FALSE;
				break;
			}
			break;
		case HKL_UNIT_ANGLE_RAD:
			switch(unit->type){
			case HKL_UNIT_ANGLE_DEG:
			case HKL_UNIT_ANGLE_RAD:
				break;
			default:
				res = HKL_FALSE;
				break;
			}
			break;
		case HKL_UNIT_LENGTH_NM:
			switch(unit->type){
			case HKL_UNIT_LENGTH_NM:
				break;
			default:
				res = HKL_FALSE;
				break;
			}
		}
	}
	return res;
}

double hkl_unit_factor(HklUnit const *self, HklUnit const *unit)
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
		}
	}
	return factor;
}
