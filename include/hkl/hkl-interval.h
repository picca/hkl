#ifndef _HKL_INTERVAL_H
#define _HKL_INTERVAL_H

#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklInterval HklInterval;

struct _HklInterval
{
	double min;
	double max;
};

int hkl_interval_cmp(HklInterval const * interval, HklInterval const * interval1);

void hkl_interval_plus_interval(HklInterval * interval, HklInterval const * interval1);

void hkl_interval_plus_double(HklInterval * interval, double const d);

void hkl_interval_minus_interval(HklInterval * interval, HklInterval const * interval1);

void hkl_interval_minus_double(HklInterval * interval, double const d);

void hkl_interval_times_interval(HklInterval * interval, HklInterval const * interval1);

void hkl_interval_times_double(HklInterval * interval, double const d);

void hkl_interval_divides_double(HklInterval * interval, double const d);

int hkl_interval_contain_zero(HklInterval const * interval);

void hkl_interval_cos(HklInterval * interval);

void hkl_interval_acos(HklInterval * interval);

void hkl_interval_sin(HklInterval * interval);

void hkl_interval_asin(HklInterval * interval);

void hkl_interval_tan(HklInterval * interval);

void hkl_interval_atan(HklInterval * interval);

HKL_END_DECLS

#endif
