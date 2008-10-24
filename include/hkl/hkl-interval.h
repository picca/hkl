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

int hkl_interval_cmp(HklInterval const *self, HklInterval const *interval);

void hkl_interval_plus_interval(HklInterval *self, HklInterval const *interval);

void hkl_interval_plus_double(HklInterval *self, double const d);

void hkl_interval_minus_interval(HklInterval *self, HklInterval const *interval);

void hkl_interval_minus_double(HklInterval *self, double const d);

void hkl_interval_times_interval(HklInterval *self, HklInterval const *interval);

void hkl_interval_times_double(HklInterval *self, double const d);

void hkl_interval_divides_double(HklInterval *self, double const d);

int hkl_interval_contain_zero(HklInterval const *self);

void hkl_interval_cos(HklInterval *self);

void hkl_interval_acos(HklInterval *self);

void hkl_interval_sin(HklInterval *self);

void hkl_interval_asin(HklInterval *self);

void hkl_interval_tan(HklInterval *self);

void hkl_interval_atan(HklInterval *self);

HKL_END_DECLS

#endif
