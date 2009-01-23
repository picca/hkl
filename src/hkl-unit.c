#include <math.h>
#include <gsl/gsl_nan.h>
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
