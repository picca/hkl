#include <hkl/hkl-diffractometer.h>
#include <hkl/hkl-diffractometer_2C.h>

/* public part */
void hkl_diffractometer_init(HklDiffractometer *diffractometer, HklDiffractometerType type, ...)
{
	double alpha;
	va_list ap;

	switch(type) {
		case HKL_DIFFRACTOMETER_2C: init_2C_diffractometer(diffractometer);
				   break;
/*
		case DIFF_TYPE_E4CV: init_E4CV_diffractometer(diff);
				     break;
		case DIFF_TYPE_E6C: init_E6C_diffractometer(diff);
				    break;
		case DIFF_TYPE_K4CV:
				    va_start(ap, type);
				    alpha = va_arg(ap, double);
				    va_end(ap);
				    init_K4CV_diffractometer(diff, alpha);
				    break;
		case DIFF_TYPE_K6C: 
				    va_start(ap, type);
				    alpha = va_arg(ap, double);
				    va_end(ap);
				    init_K6C_diffractometer(diff, alpha);
				    break;
*/
		default:
			die("Not a known diffractometer");
	}
}

void hkl_diffractometer_release(HklDiffractometer * diffractometer)
{
	hkl_geometry_release(&diffractometer->geometry);
}
